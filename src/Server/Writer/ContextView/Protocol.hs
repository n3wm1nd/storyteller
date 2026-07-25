{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Protocol for @\/branch\/{name}\/$context\/{path}@ connections.
--
-- Commands: 'PreviewContext' — a full, self-contained Context DSL program
--           to run against the branch, plus the @path@ it should resolve
--           against (the same @path:@ parameter every @context.writer@
--           call takes). Every request carries everything needed to answer
--           it, the same discipline an LLM call's full history follows:
--           nothing about a submitted program persists across requests, so
--           a client never needs to reconstruct or diff against
--           server-held state.
--
--           'EstimateCost' — the same @(path, program)@ shape, asking
--           instead for a per-line size breakdown (see
--           'Storyteller.Writer.Agent.ContextCost'). Kept as its own
--           command rather than folded into every 'PreviewContext'
--           response: cost estimation runs the whole program once per
--           candidate line (ablation, not static instrumentation — see
--           that module's own Haddock), materially more expensive than a
--           single resolve, so a client asks for it only when it actually
--           wants to see where the budget is going, not on every
--           branch-change-triggered re-preview.
--
--           'EstimateAdhocCost' — the same idea, but for a bare 0-arity
--           snippet with no @path@ of its own (see
--           'Storyteller.Writer.Agent.ContextCost.buildAdhocProgramCosts')
--           -- what a @pinnedPrograms@ entry
--           (Server.Writer.File.Protocol's own @ChatWriter@) actually is,
--           now that @context.writer@ no longer accepts a whole-program
--           override to estimate against (see the project chat that
--           settled the writer context's three-slot model).
--
--           'PreviewAdhoc' — 'PreviewContext''s own bare-snippet
--           counterpart, via 'Storyteller.Writer.Agent.ContextPreview.
--           buildAdhocPreview': what a named context slot's own editor
--           (a project-default @context.lore@\/@context.chaptersCompressed@
--           override, or a saved pinned snippet) actually previews against
--           -- resolved and rendered directly, never staged as a whole
--           @context.writer@ override the way 'PreviewContext' works, since
--           a slot editor isn't editing the writer's entire context.
--
--           'EntriesFor' — the flat file-path list a named 0-arity or
--           1-arity context slot (@context.lore@, @context.other@)
--           currently resolves to, via
--           'Storyteller.Writer.Agent.ContextPreview.buildEntries0'\/
--           'buildEntries1' -- what a casual file-toggle list (e.g.
--           "which lore files are currently included") reads, live, so it
--           can never drift from what a real @chat.writer@ send would
--           actually include (see those functions' own Haddock). @path@ is
--           optional -- present for a 1-arity slot like @context.other@,
--           omitted for a 0-arity one like @context.lore@; sending it for
--           a 0-arity slot is a no-op, not an error, since 'buildEntries0'
--           simply never asks for it.
-- Events:   'ContextPreviewed' — the resolved tree, pushed once per
--           'PreviewContext'\/'PreviewAdhoc' and (for 'PreviewContext'
--           only) again whenever the underlying branch changes
--           (re-resolved against the most recently submitted program —
--           see 'Server.Writer.ContextView.Connection').
--
--           'ContextCosted' — one 'LineCost' per ablation candidate,
--           pushed once per 'EstimateCost'\/'EstimateAdhocCost'. Not
--           re-pushed on every branch change the way 'ContextPreviewed'
--           is (see 'Server.Writer.ContextView.Connection': only the most
--           recently submitted *preview* request is remembered for that
--           purpose) — a cost estimate is a deliberate, one-off "show me
--           now" action, not a live-updating view.
--
--           'ContextEntries' — the resolved path list, pushed once per
--           'EntriesFor' and, unlike 'ContextCosted' but like
--           'ContextPreviewed', again whenever the underlying branch
--           changes -- a file-toggle list needs to reflect a file
--           added/removed/renamed on the branch live, the same reason
--           'ContextPreviewed' itself re-pushes (see
--           'Server.Writer.ContextView.Connection').
module Server.Writer.ContextView.Protocol
  ( PreviewNode(..)
  , LineCost(..)
  , ContextViewCommand(..)
  , ContextViewEvent(..)
  ) where

import Data.Aeson hiding (Error)
import Data.Aeson.Types (Parser)
import qualified Data.Text as T

import Storyteller.Writer.Agent.ContextCost (LineCost(..))
import Storyteller.Writer.Agent.ContextPreview (PreviewNode(..))

instance ToJSON PreviewNode where
  toJSON n = object
    [ "content" .= pnContent n
    , "entries" .= [ object [ "name" .= name, "node" .= toJSON node ] | (name, node) <- pnEntries n ]
    ]

instance ToJSON LineCost where
  toJSON lc = object
    [ "line"  .= lcLine lc
    , "col"   .= lcCol lc
    , "chars" .= lcChars lc
    ]

-- | Commands the client may send on a context-view connection.
data ContextViewCommand
  = PreviewContext { cvId :: Maybe T.Text, cvPath :: FilePath, cvProgram :: T.Text }
  | PreviewAdhoc { cvId :: Maybe T.Text, cvProgram :: T.Text }
  | EstimateCost { cvId :: Maybe T.Text, cvPath :: FilePath, cvProgram :: T.Text }
  | EstimateAdhocCost { cvId :: Maybe T.Text, cvProgram :: T.Text }
  | EntriesFor { cvId :: Maybe T.Text, cvName :: T.Text, cvEntriesPath :: Maybe FilePath }
  deriving (Show)

instance FromJSON ContextViewCommand where
  parseJSON = withObject "ContextViewCommand" $ \o -> do
    t <- o .: "type" :: Parser T.Text
    i <- o .:? "id"
    case t of
      "context.preview"       -> PreviewContext i <$> o .: "path" <*> o .: "program"
      "context.preview.adhoc" -> PreviewAdhoc i <$> o .: "program"
      "context.cost"          -> EstimateCost  i <$> o .: "path" <*> o .: "program"
      "context.cost.adhoc"    -> EstimateAdhocCost i <$> o .: "program"
      "context.entries"       -> EntriesFor i <$> o .: "name" <*> o .:? "path"
      _                       -> fail ("unknown context-view command: " <> T.unpack t)

-- | Events the server sends on a context-view connection.
data ContextViewEvent
  = ContextPreviewed { cveId :: Maybe T.Text, cveResult :: PreviewNode }
  | ContextCosted { cveId :: Maybe T.Text, cveCosts :: [LineCost] }
  | ContextEntries { cveId :: Maybe T.Text, cveEntries :: [T.Text] }
  | ContextViewError T.Text
  deriving (Show)

instance ToJSON ContextViewEvent where
  toJSON = \case
    ContextPreviewed mid result ->
      object $
        [ "type"   .= ("context.preview" :: T.Text)
        , "result" .= result
        ] <> maybe [] (\i -> ["id" .= i]) mid
    ContextCosted mid costs ->
      object $
        [ "type"  .= ("context.cost" :: T.Text)
        , "costs" .= costs
        ] <> maybe [] (\i -> ["id" .= i]) mid
    ContextEntries mid entries ->
      object $
        [ "type"    .= ("context.entries" :: T.Text)
        , "entries" .= entries
        ] <> maybe [] (\i -> ["id" .= i]) mid
    ContextViewError msg ->
      object [ "type" .= ("error" :: T.Text), "message" .= msg ]
