{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Per-line cost estimation for a Context DSL program, by ablation
--   rather than instrumentation: for each statement in the program, blank
--   it out (replace it with a no-op that contributes nothing), re-run the
--   *whole* program exactly the way 'Storyteller.Writer.Agent.ContextPreview'
--   already does, and measure how much smaller the rendered output got.
--   @cost(line) = size(baseline) - size(with line ablated)@.
--
--   Deliberately not a static walk that sums up "what each statement's
--   own 'Storyteller.Context.DSL.Value.Value' would render to in
--   isolation" -- that would silently disagree with the real total the
--   moment any filter downstream does something non-additive (@exclude@
--   shrinking a key set another statement also touches, @sortBy@
--   reordering, a character's blurb getting deduplicated against
--   something already pulled in some other way, ...). Ablation costs
--   nothing to keep correct under filter changes because it never
--   inspects what a filter does -- it only ever compares two real runs of
--   the actual pipeline, so whatever a filter does downstream of a line
--   is automatically included in that line's own measured contribution.
--
--   The real cost is running the whole program once per candidate line
--   (@n+1@ full evaluations for @n@ statements) -- fine for a
--   user-triggered "estimate costs" action over a program of ordinarily a
--   few dozen statements against filters that are themselves cheap pure
--   transforms or simple storage reads, not something to run on every
--   keystroke or every branch-change notification the way
--   'Storyteller.Writer.Agent.ContextPreview.buildPreview' is re-triggered.
module Storyteller.Writer.Agent.ContextCost
  ( LineCost(..)
  , buildLineCosts
  , buildProgramCosts
  , buildAdhocProgramCosts
  ) where

import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Polysemy (Members, Sem)
import Polysemy.Fail (Fail)

import qualified Data.Map.Strict as Map

import Storyteller.Context.DSL.AST (Block, Definition(..), Located(..), Pos(..), Stmt(..))
import Storyteller.Context.DSL.Compile (Binding, Library, runDefinition)
import Storyteller.Context.DSL.Library (contextWriterDef)
import Storyteller.Context.DSL.Parser (parseDefinition)
import Storyteller.Context.DSL.Rendering (renderContext, renderText)
import Storyteller.Context.DSL.Value (Message(User), bval, leafValue)
import Storyteller.Core.Branch (BranchOp, Branches)
import Storyteller.Core.Context
  ( ContextRow, ContextStorage, adhocArgs, buildContextLibrary, getContextOverrides
  , resolveOverrideDefinition, runContextValue
  )

-- | One statement's own measured contribution -- @posLine@\/@posCol@
--   identify exactly which source statement this is (a 'Pos' is unique
--   per statement: no two ever start at the same line\/column), so a
--   caller matches this back against the program text it already has
--   without this module needing to carry source spans of its own.
data LineCost = LineCost
  { lcLine  :: !Int
  , lcCol   :: !Int
  , lcChars :: !Int
    -- ^ @size(baseline) - size(with this statement ablated)@, in
    --   rendered characters (i.e. what 'Storyteller.Context.DSL.Rendering.renderText'
    --   produces -- the same text a real send would build a token count
    --   from). Can be negative in principle (a statement whose removal
    --   somehow makes a downstream filter emit *more* -- no filter in
    --   'Storyteller.Context.DSL.Compile.coreFilters' actually behaves
    --   this way today, but ablation makes no assumption that rules it
    --   out), though ordinarily zero or positive.
  } deriving (Show, Eq)

-- | Every statement position in @block@ that's actually a candidate to
--   ablate, recursing into every nested block ('SAs'\/'SLet'\/'SIn'\/'SFor'
--   each carry one) -- candidates aren't just top-level lines; a @for@
--   loop's own body statement is exactly as real a "line" a user wrote as
--   anything at the top level.
--
--   'SLet''s own position is deliberately *not* a candidate, unlike every
--   other statement shape -- a binding itself never emits anything and
--   costs nothing on its own; only a later reference to it
--   ('SExpr (EIdent _)', or the name used inside an @as@\/@for@) is what
--   actually contributes size. Ablating the binding wholesale would
--   dangle every one of those later references (an "unknown identifier"
--   evaluation failure, not a shrink), which is the wrong question to
--   even ask: "what does defining @x@ cost" isn't well-formed when the
--   real cost lives entirely at each call site. 'SLet''s own *body* is
--   still walked into and offered as ordinary candidates (a bound
--   function like @x = f: read f@ can itself contain real, ablatable
--   statements) -- only the enclosing 'SLet' position itself is skipped.
positions :: Block -> [Pos]
positions = concatMap onLocated
  where
    onLocated (Located pos stmt) = case stmt of
      SLet _ _ body -> positions body
      _             -> pos : onStmt stmt
    onStmt = \case
      SExpr _     -> []
      SAs _ body  -> positions body
      SLet _ _ body -> positions body
      SIn _ body  -> positions body
      SFor _ _ body -> positions body

-- | @block@ with the statement at @target@ dropped outright -- not
--   replaced by a neutral stand-in, which sounds equivalent but isn't:
--   'Storyteller.Context.DSL.Rendering.renderText' joins every element of
--   a node's own rendered content with a two-character @"\\n\\n"@
--   separator, so substituting an empty-string 'SExpr' for, say, a whole
--   @as@\/@for@\/@in@ statement (neither of which contributes to the
--   enclosing default at all -- 'SAs' only ever adds a named *entry*,
--   'SFor'\/'SIn' fold their body's own emissions in directly) would
--   still add one *more* element to that content list, one more
--   separator's worth of padding included, even though its own text is
--   empty -- silently overcounting that statement's own cost by up to 2.
--   Filtering it out of the block entirely has no such asymmetry:
--   'runStmts' never sees the statement at all, the identical effect
--   deleting the line from the source would have.
--
--   Every other statement, and every nested block that doesn't itself
--   contain @target@, is left untouched -- only the one exact position
--   named is ever dropped, so ablating one @for@-body line doesn't also
--   silence its siblings.
ablate :: Pos -> Block -> Block
ablate target block =
  [ Located pos (onStmt stmt)
  | Located pos stmt <- block
  , pos /= target
  ]
  where
    onStmt = \case
      SAs nameE body    -> SAs nameE (ablate target body)
      SLet n ps body    -> SLet n ps (ablate target body)
      SIn scopeE body   -> SIn scopeE (ablate target body)
      SFor n srcE body  -> SFor n srcE (ablate target body)
      s@(SExpr _)       -> s

-- | The rendered size of running @def@ against @lib@ with @args@ already
--   bound -- 'renderText' char count, the same text a real send's token
--   estimate would start from. @args@ is whatever the caller's own
--   definition declares (a single @path@ binding for @context.writer@-
--   shaped programs via 'buildProgramCosts', none at all for a bare
--   0-arity snippet via 'buildAdhocProgramCosts') -- this function itself
--   has no opinion on arity, same as 'runDefinition' doesn't.
sizeOf :: forall branch r. Members '[BranchOp branch, Branches, ContextStorage, Fail] r => Library (ContextRow r) -> Definition -> [Binding (ContextRow r)] -> Sem r Int
sizeOf lib def args =
  runContextValue @branch $ do
    v        <- runDefinition lib def args
    rendered <- renderContext v
    pure (T.length (renderText rendered))

-- | Every statement in @def@'s own body, each paired with its own
--   ablation cost against @args@ -- the whole point of this module.
--   Evaluates the program @1 + length (positions (defBody def))@ times
--   total (once for the real baseline, once per candidate line), each a
--   fresh, independent 'runContextValue' interpretation exactly like a
--   real preview\/send would use, so nothing here shares state across
--   runs that could leak a stale cascade or cached branch position from
--   one measurement into the next.
--
--   Sorted by descending cost -- what a user actually wants first is
--   "what's eating my budget," not source order.
buildLineCosts
  :: forall branch r
  .  Members '[BranchOp branch, Branches, ContextStorage, Fail] r
  => Library (ContextRow r) -> Definition -> [Binding (ContextRow r)] -> Sem r [LineCost]
buildLineCosts lib def args = do
  baseline <- sizeOf @branch lib def args
  let candidates = positions (defBody def)
  costs <- mapM (\pos -> do
    ablatedSize <- sizeOf @branch lib def { defBody = ablate pos (defBody def) } args
    pure LineCost { lcLine = posLine pos, lcCol = posCol pos, lcChars = baseline - ablatedSize }
    ) candidates
  pure (sortOn (negate . lcChars) costs)

-- | The entry point a WS connection actually calls -- same shape and
--   staging convention as 'Storyteller.Writer.Agent.ContextPreview.buildPreview'
--   (@program@ is a full, self-contained @context.writer@ override, run
--   against @path@), so "preview this program" and "estimate its line
--   costs" can never disagree about what the program even resolves to: a
--   broken @program@ (parse failure, or an arity mismatch against
--   whatever else in the library still calls @context.writer@) is
--   rejected by 'Storyteller.Core.Context.buildContextLibrary' itself,
--   the identical check\/fallback a real send would apply, rather than
--   this module inventing its own, second notion of "invalid program."
--   'buildLineCosts' still needs the actual 'Definition' (line-level
--   ablation walks its parsed body), not just the ability to run it, so
--   this reads @overrides'@'s own rejection list to tell "@program@ was
--   accepted" apart from "the compiled-in default is what's actually
--   live" -- the same verdict 'buildContextLibrary' already reached,
--   never a second, independent check of @program@ on its own.
buildProgramCosts
  :: forall branch r
  .  Members '[BranchOp branch, Branches, ContextStorage, Fail] r
  => FilePath -> Text -> Sem r [LineCost]
buildProgramCosts path program = do
  overrides <- getContextOverrides
  let overrides'         = Map.insert "context.writer" program overrides
      (table, rejected)  = buildContextLibrary @branch overrides'
      def = if "context.writer" `elem` rejected
              then contextWriterDef
              else fromMaybe contextWriterDef (either (const Nothing) Just (parseDefinition "<context override>" program))
  buildLineCosts @branch table def [bval (pure (leafValue [User (T.pack path)]))]

-- | 'buildLineCosts', but for a bare 0-arity ad-hoc snippet -- what a
--   @pinnedPrograms@ entry (see Server.Writer.File.Protocol's own
--   Haddock) actually is: no @path@, no slot identity, just "this
--   program's own rendered size, broken down per statement." The
--   frontend's context-cost-sidebar.tsx repoints here (rather than
--   'buildProgramCosts') now that @context.writer@ no longer accepts a
--   whole-program override to estimate against -- see the project chat
--   that settled the writer context's three-slot model. A parse failure
--   is a real error here (mirroring 'Storyteller.Core.Context.resolveAdhoc':
--   there's no slot default to silently fall back to for a program that
--   was never a named slot to begin with).
--
--   @mPath@ is the editing surface's own "which file would this run
--   against", matched against the program's declared parameters by the
--   shared 'Storyteller.Core.Context.adhocArgs' rule -- so a @path:@-headed
--   program (a custom agent's, @context.other@'s) costs correctly instead
--   of being rejected outright for having an argument, which is what used
--   to make its estimate come back empty. See
--   'Storyteller.Writer.Agent.ContextPreview.buildAdhocPreview', which
--   takes the identical argument for the identical reason -- the two must
--   agree about what they're measuring.
buildAdhocProgramCosts
  :: forall branch r
  .  Members '[BranchOp branch, Branches, ContextStorage, Fail] r
  => Text -> Maybe FilePath -> Sem r [LineCost]
buildAdhocProgramCosts program mPath = do
  overrides <- getContextOverrides
  let (table, _rejected) = buildContextLibrary @branch overrides
  case resolveOverrideDefinition (Just program) of
    Nothing  -> fail ("buildAdhocProgramCosts: not a valid program: " <> T.unpack program)
    Just def -> case adhocArgs def (maybe [] (pure . T.pack) mPath) of
      Left err   -> fail ("buildAdhocProgramCosts: " <> err)
      Right args -> buildLineCosts @branch table def [ bval (pure (leafValue [User a])) | a <- args ]
