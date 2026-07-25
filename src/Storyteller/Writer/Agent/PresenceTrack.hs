{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Retroactive presence tracking: given a scene's whole prose and the
--   story's known cast, ask the model which characters enter or leave over
--   the course of it, and record the result as ordinary
--   'Storyteller.Writer.Presence.recordPresenceAtAtom' ticks, each inserted at
--   the exact chain position it actually happened — the same tick kind
--   'Server.Writer.File.setPresence' writes by hand today, just placed
--   where it actually belongs instead of always at the branch's current
--   head. This is the bulk-ingestion counterpart to that manual UI action:
--   for a work written before presence tracking existed (or one imported
--   wholesale), nobody ever clicked "enter"/"leave" for any character, so
--   there is nothing for 'Storyteller.Writer.Presence.activeCharactersFor'
--   to fold over.
--
--   __Why the model names a quoted span, not an atom.__ "Atom" is a storage
--   concept the model has no business knowing about — see
--   @EFFECTS.md@\/@STRUCTURE.md@'s "erring toward specificity" on agent
--   authors (and by extension a model itself) never needing to think in
--   storage internals. The model reads the *whole* scene once, the way a
--   person would, and each @mark_enter@\/@mark_leave@ call names the
--   character plus a short, exact, uniquely-identifying quote from the text
--   at the point the event happens (@at_text@) — 'resolveAnchor' is what
--   turns that quote into a real chain position afterward, entirely the
--   caller's problem, not the model's. This also means the model is only
--   ever queried once per file, not once per atom: no repeated re-sending
--   of the whole scene, and nothing about atom boundaries needs to survive
--   into the prompt or the tool schema at all.
--
--   __Why a chain position, not just a same-file tag.__ A live, in-progress
--   scene naturally gets its presence ticks recorded at the branch's
--   current head ('Server.Writer.File.setPresence' is clicked turn by turn
--   as the scene is actually written, so "head" and "the correct chain
--   position" are the same point at the moment of the click) — but
--   retroactive ingestion of an already-fully-written, multi-atom chapter
--   has no such luxury: a character who enters partway through has to be
--   marked present starting at the specific point where they actually
--   appear, not from the whole file's start, or a later reader replaying
--   history at an earlier atom (see 'Storyteller.Writer.Presence.presentAt')
--   would see them there too, when they aren't.
--
--   'resolveAnchor' does the placement: it finds the one atom
--   (in @path@'s own tick chain) whose text contains @at_text@ — failing,
--   not guessing, if that match is missing or ambiguous, the same
--   exact-match-once discipline
--   'Storyteller.Writer.Agent.ReplaceTool.replaceOnce'\/@Runix.Tools.editFile@
--   already apply to a model-supplied span — then hands the matched atom's
--   own tick id to 'Storyteller.Writer.Presence.recordPresenceAtAtom',
--   which decides Enter-before\/Leave-after placement itself.
--
--   'presenceAgent' is the pure decision core, same shape as
--   'Storyteller.Writer.Agent.ReplaceTool.reworkAtom': only 'LLM'\/'Fail',
--   no filesystem or storage access, easy to test/tune in isolation. It
--   gives the model two tools, @mark_enter@\/@mark_leave@, rather than
--   asking for one structured list in a single response — see
--   @test/agent-integration/PLAN.md@\/@FINDINGS.md@'s tool-call-loop
--   findings: this is exactly the "enumerate zero or more items from a
--   fixed candidate set" shape 'Storyteller.Writer.Agent.Outline.
--   splitOutlineAgent' already established the pattern for (one call per
--   event, looped across turns until the model stops), and giving the
--   model the full cast up front means it only ever has to name characters
--   it actually recognizes rather than transcribing long names correctly
--   inside a single bulk JSON blob.
--
--   'trackPresenceFor' is the effectful wrapper: reads the scene's whole
--   current text and the story's full cast (via
--   'Storyteller.Core.ContentEffects.knownCast'), hands both to
--   'presenceAgent', and resolves\/applies every decision via
--   'resolveAnchor'\/'Storyteller.Writer.Presence.recordPresenceAtAtom'. Safe
--   to re-run: a decision that's already reflected in the file's state as
--   of its own anchor is a no-op there (see that function's own Haddock),
--   so nothing here needs to track "have I already processed this file"
--   itself.
--
--   'trackPresenceForAll' is the bulk driver "run over ... all chapters"
--   asks for: every recognized prose unit in reading order (via
--   'Storyteller.Writer.Library.narrativeUnits'), oldest first, so a
--   character introduced in chapter 1 is already on record by the time
--   chapter 3 is read — the model is never asked to infer presence for a
--   later chapter without seeing what came before.
module Storyteller.Writer.Agent.PresenceTrack
  ( PresenceDecision(..)
  , presenceAgent
  , trackPresenceFor
  , trackPresenceForAll
  ) where

import Data.Aeson.Types (parseEither)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Polysemy
import Polysemy.Fail (Fail)

import Autodocodec (HasCodec(..), dimapCodec, object, requiredField, parseJSONViaCodec, (.=))
import Runix.FileSystem (FileSystem, FileSystemRead, fileExists, listAllFiles, readFile)
import Runix.LLM (queryLLM)
import Runix.LLM.ToolExecution (executeTool)
import Runix.LLM.ToolInstances ()
import Runix.Logging (Logging, info, warning)
import Storage.Tick (FileTick(..))
import UniversalLLM (Message(..), ModelConfig(..))
import UniversalLLM.Tools
  ( ToolParameter(..), LLMTool(..), mkToolWithMeta, llmToolToDefinition
  , ToolResult(..)
  )

import Storyteller.Core.ContentEffects (Cast, CastMember(..), fileTicksOf, knownCast, runFileTicks)
import Storyteller.Core.Git (BranchOp, BranchTag)
import Storyteller.Core.LLM.Role (LLMs, AgentModel)
import Storyteller.Core.Prompt (Prompt(..), PromptStorage, getPrompt, getConfigWithPrompt)
import Storyteller.Core.Storage (StoryStorage)
import Storyteller.Core.Types (BranchName(..), TickId(..))
import Storyteller.Writer.Branches (branchDisplayName)
import Storyteller.Writer.Library (buildLibraryTree, narrativeUnits, UnitInfo(..))
import Storyteller.Writer.Presence (activeCharactersFor, recordPresenceAtAtom)
import Storyteller.Writer.Types (Character(..), PresenceEvent(..))

import Prelude hiding (readFile)

-- ---------------------------------------------------------------------------
-- The pure decision core
-- ---------------------------------------------------------------------------

-- | One decision the model made: a character's presence event, anchored to
--   a short, exact quote from the scene ('pdAtText') rather than any
--   storage position — see the module Haddock for why. Resolving that quote
--   to a real chain position is 'resolveAnchor's job, not this type's.
data PresenceDecision = PresenceDecision
  { pdCharacter :: BranchName
  , pdEvent     :: PresenceEvent
  , pdAtText    :: T.Text
  } deriving (Show, Eq)

instance HasCodec PresenceDecision where
  codec = object "PresenceDecision" $
    PresenceDecision
      <$> (BranchName <$> requiredField "character" "the character's branch id") .= (unBranchName . pdCharacter)
      <*> (eventFromBool <$> requiredField "enter" "True for enter, False for leave") .= ((== Enter) . pdEvent)
      <*> requiredField "at_text" "the exact quoted text this event happens at" .= pdAtText
    where
      eventFromBool b = if b then Enter else Leave

instance ToolParameter PresenceDecision where
  paramName = "presence_decision"
  paramDescription = "the character, whether they entered or left, and where in the text"

newtype CharacterId = CharacterId T.Text
instance HasCodec CharacterId where
  codec = dimapCodec CharacterId (\(CharacterId t) -> t) codec
instance ToolParameter CharacterId where
  paramName = "character"
  paramDescription = "the character's exact name from the cast list"

newtype AtText = AtText T.Text
instance HasCodec AtText where
  codec = dimapCodec AtText (\(AtText t) -> t) codec
instance ToolParameter AtText where
  paramName = "at_text"
  paramDescription = "a short, exact, word-for-word quote from the scene text, marking where this happens"

-- | @mark_enter@\/@mark_leave@'s own function: resolves the model's
--   @character@ argument (a display name, e.g. @"rennick"@ -- see
--   'presenceAgent's own Haddock on why it's the display name and not the
--   full branch id) against @cast@ and packages the result, together with
--   the model's quoted @at_text@, as a 'PresenceDecision' — the tool's
--   result *is* the structured answer, the same shape
--   'Storyteller.Writer.Agent.Outline.emitBeatSheet' and
--   'Storyteller.Writer.Agent.ReplaceTool.proposeReplacement' both already
--   use.
--
--   A name that doesn't match any cast member genuinely fails
--   ('Polysemy.Fail.fail'), not a silently-dropped 'Nothing' — the model
--   invented or misspelled a name (it was given the exact roster), and
--   needs to see that as an error and get a chance to correct it, the same
--   way 'Storyteller.Writer.Agent.ReplaceTool.reworkAtom''s tools and
--   'Agent.Integration.Judge.judge' both feed a bad call back rather than
--   quietly discarding it. 'presenceAgent's loop runs this through
--   'Runix.LLM.ToolExecution.executeTool' rather than plain
--   'UniversalLLM.Tools.executeToolCallFromList', specifically so this
--   'fail' becomes a real 'UniversalLLM.Tools.ToolResult' the model sees
--   (@Left errMsg@) instead of aborting the whole scenario -- see that
--   function's own Haddock. Whether @at_text@ actually matches anything in
--   the scene is deliberately *not* checked here -- this function has no
--   access to the scene text at all, only 'resolveAnchor' (which does, and
--   which runs later, against real storage) can judge that.
markPresence :: forall r. Members '[Fail] r => [CastMember] -> PresenceEvent -> CharacterId -> AtText -> Sem r PresenceDecision
markPresence cast event (CharacterId ident) (AtText atText) =
  case lookup ident [ (branchDisplayName (unBranchName (cmBranch cm)), cm) | cm <- cast ] of
    Just cm -> pure (PresenceDecision (cmBranch cm) event atText)
    Nothing -> fail $ "no character named \"" <> T.unpack ident <> "\" in the cast list -- use one of the exact names given"

-- | Ask the model which cast members enter or leave over the course of
--   @sceneText@ (the scene's whole current prose, read once — see the
--   module Haddock for why this isn't per-atom), given who's already
--   present going in (@alreadyPresent@ — so the model isn't asked to
--   re-declare an Enter for someone already in the scene, only genuine
--   transitions) and the full @cast@ to recognize mentions against. Only
--   'LLM'\/'Fail' — no filesystem or storage access, same as
--   'Storyteller.Writer.Agent.ReplaceTool.reworkAtom'; resolving each
--   decision's quoted @at_text@ to a real position and applying it are the
--   caller's job ('resolveAnchor'\/'trackPresenceFor').
--
--   Two tools, @mark_enter@\/@mark_leave@, each taking @character@ and
--   @at_text@ — see the module Haddock for why this is a tool-call loop
--   (mirroring 'Storyteller.Writer.Agent.Outline.splitOutlineAgent') rather
--   than one bulk structured response. Run through
--   'Runix.LLM.ToolExecution.executeTool' rather than plain
--   'UniversalLLM.Tools.executeToolCallFromList', so 'markPresence''s own
--   validation failure (an unrecognized name) reaches the model as a real,
--   correctable tool error instead of being silently dropped or aborting
--   the whole call -- see 'markPresence's own Haddock.
presenceAgent
  :: forall r
  .  (LLMs r, Members '[PromptStorage, Fail, Logging] r)
  => [CastMember] -> [BranchName] -> T.Text -> Sem r [PresenceDecision]
presenceAgent cast alreadyPresent sceneText = do
  configsWithPrompt <- getConfigWithPrompt "agent.presence" defaultPresenceSystemPrompt defaultPresenceConfig
  Prompt closing    <- getPrompt "agent.presence.instructions" defaultPresenceInstructions

  let atTextParam = ("at_text", "a short, exact, word-for-word quote from the scene text, unique within it, \
                                 \marking the point where this happens -- e.g. the sentence they first appear \
                                 \or last appear in")
      enterTool = mkToolWithMeta
        "mark_enter"
        "Record that a character enters or is already present, anchored to where in the text this becomes \
        \true -- call once per character who is on-page at any point, if they aren't already marked present."
        (markPresence @(Fail ': r) cast Enter)
        "character" "the character's exact name from the cast list, e.g. \"rennick\""
        (fst atTextParam) (snd atTextParam)
      leaveTool = mkToolWithMeta
        "mark_leave"
        "Record that a character leaves the scene before it ends, anchored to where in the text this becomes \
        \true -- only call this for a character who was present and then departs; do not call it for a \
        \character who simply never appears."
        (markPresence @(Fail ': r) cast Leave)
        "character" "the character's exact name from the cast list, e.g. \"rennick\""
        (fst atTextParam) (snd atTextParam)
      tools = [LLMTool enterTool, LLMTool leaveTool]
      allConfigs = Tools (map llmToolToDefinition tools) : configsWithPrompt

      -- The name given to the model, and the one 'markPresence' resolves a
      -- call's @character@ argument against, is the display name
      -- ('Storyteller.Writer.Branches.branchDisplayName') -- "rennick", not
      -- the full "character/rennick" branch path. A model asked to name a
      -- character naturally reaches for how a person would refer to them,
      -- not an internal storage path; an earlier version of this listed the
      -- full branch id as the primary identifier and every tool call came
      -- back using the display name shown in parentheses next to it
      -- instead, silently dropped by a filter that only matched the full
      -- id -- confirmed against a live cached run
      -- (@Agent.Integration.CharacterPresenceTrackSpec@).
      castSection
        | null cast = "The story has no character branches yet -- there is nobody to mark present."
        | otherwise = "Known cast (call the tools with these exact names):\n"
            <> T.unlines [ "- " <> displayNameOf cm <> renderSheet cm | cm <- cast ]
      presentSection
        | null alreadyPresent = "Nobody is currently marked present at the start of this scene."
        | otherwise = "Already present at the start of this scene: "
            <> T.intercalate ", " (map displayNameOf' alreadyPresent)

      userMsg = T.intercalate "\n\n"
        [ castSection, presentSection, "Scene text:\n\n" <> sceneText, closing ]

  info "presenceAgent: reviewing scene for character entrances/exits..."
  decisions <- loop tools allConfigs (1 :: Int) [UserText userMsg]
  info $ "presenceAgent: done, " <> T.pack (show (length decisions)) <> " decision(s)"
  return decisions
  where
    displayNameOf  = branchDisplayName . unBranchName . cmBranch
    displayNameOf' = branchDisplayName . unBranchName

    renderSheet cm
      | T.null (T.strip (cmSheet cm)) = ""
      | otherwise = " -- " <> T.strip (cmSheet cm)

    -- Same turn-by-turn execute-and-recurse shape as 'splitOutlineAgent':
    -- one queryLLM only ever sees the calls from its own turn, so several
    -- characters entering/leaving needs several turns fed back as tool
    -- results, not one call. Bounded directly (not via 'withTurnBudget',
    -- which only fires on a text-turn sentinel, never reached here since
    -- this loop stops as soon as a turn makes no tool calls at all).
    loop _ _ turnNo _ | turnNo > maxTurns = do
      warning $ "presenceAgent: hit the " <> T.pack (show maxTurns) <> "-turn budget without the model settling, stopping"
      return []
    loop tools allConfigs turnNo history = do
      info $ "presenceAgent: turn " <> T.pack (show turnNo) <> ": querying model..."
      response <- queryLLM allConfigs history
      let calls = [tc | AssistantTool tc <- response]
      if null calls
        then return []
        else do
          executed <- mapM (executeTool tools) calls
          let decisions = mapMaybe harvest executed
              history'  = history <> response <> map ToolResultMsg executed
          (decisions <>) <$> loop tools allConfigs (turnNo + 1) history'

    -- Cast-membership validation already happened inside 'markPresence' --
    -- a call that reached here successfully names a real cast member, so
    -- this is purely decoding, not a second filter. Whether @at_text@
    -- resolves to anything is checked later, by 'resolveAnchor'.
    harvest (ToolResult _ (Right value)) = either (const Nothing) Just (parseEither parseJSONViaCodec value)
    harvest _ = Nothing

    maxTurns = 40 :: Int

defaultPresenceSystemPrompt :: Prompt
defaultPresenceSystemPrompt = Prompt $ T.unlines
  [ "You are reading one scene of a story to work out which characters are physically present in it --"
  , "who is on-page, from the moment they first appear to the moment they leave (if they do). This is"
  , "retroactive bookkeeping for a scene that already exists, not a creative task: don't invent"
  , "characters, don't guess at anyone not actually named or unambiguously referred to in the text."
  ]

defaultPresenceInstructions :: Prompt
defaultPresenceInstructions = Prompt $ T.unlines
  [ "For every cast member who is on-page at any point in this scene and not already marked present, call"
  , "mark_enter once with their exact name and at_text set to a short, exact, word-for-word quote from the"
  , "scene marking the point they become present -- their first appearance if it's not the very start of"
  , "the scene, or any short quote from the opening if they're already there when the scene begins. For"
  , "every cast member who is present and then clearly leaves before the scene ends, call mark_leave once"
  , "the same way, with at_text quoting the point they leave. at_text must be copied exactly, word for"
  , "word, from the scene text, and must be unique within it -- long and specific enough that it couldn't"
  , "match more than one place. Call nothing for a character who doesn't appear at all, and don't call"
  , "mark_enter again for someone already marked present. Once you've made every call this scene warrants,"
  , "stop -- there's nothing else to say."
  ]

-- | Reading a whole scene plus a full cast list is comparable to
--   'Storyteller.Writer.Agent.Outline.defaultSplitConfig's own budget
--   reasoning -- kept well above what the handful of short tool calls this
--   agent makes actually need, since a reasoning-capable model's thinking
--   tokens draw from the same budget (see that module's Haddock for the
--   general shape of this finding). Low temperature: this is bookkeeping
--   against the text as given, not creative generation.
defaultPresenceConfig :: [ModelConfig AgentModel]
defaultPresenceConfig = [MaxTokens 4096, Temperature 0.2]

-- ---------------------------------------------------------------------------
-- Resolving a quoted span to a real chain position
-- ---------------------------------------------------------------------------

-- | Turn one 'PresenceDecision''s quoted 'pdAtText' into a real atom and
--   apply it via 'Storyteller.Writer.Presence.recordPresenceAtAtom' --
--   which decides before-vs-after placement from the event itself (see its
--   own Haddock); this only has to find *which* atom. Finds the one atom
--   (from @ticks@ -- typically
--   'Storyteller.Core.ContentEffects.fileTicksOf's own result) whose text
--   contains 'pdAtText'; fails loudly (logged, not applied) if the match is
--   missing or ambiguous across atoms, the same "an unresolvable
--   model-supplied span is a real problem, not silently ignorable" stance
--   'Storyteller.Writer.Agent.ReplaceTool.replaceOnce' takes for a
--   within-one-atom span (this is the cross-atom, "which atom" version of
--   the same judgement call).
--
--   Matched with whitespace normalized on both sides ('squashWhitespace') --
--   the model reads prose the way a person does, soft-wrapped, and quotes a
--   span the same way, with no reason to reproduce the source's exact line
--   breaks; a literal byte-for-byte match would spuriously fail whenever a
--   quote happens to straddle a wrap point in the stored text, even though
--   the quote is a perfectly correct, unambiguous identification of the
--   right span.
resolveAnchor
  :: forall branch r
  .  Members '[BranchOp branch, StoryStorage, Fail, Logging] r
  => FilePath -> [FileTick] -> PresenceDecision -> Sem r ()
resolveAnchor path ticks (PresenceDecision charBranch event atText) =
  case [ ft | ft <- atoms, Just t <- [ftContent ft], squashWhitespace atText `T.isInfixOf` squashWhitespace t ] of
    [matched] -> do
      info $ "resolveAnchor: " <> T.pack path <> ": " <> unBranchName charBranch <> " "
           <> T.pack (show event) <> " at \"" <> atText <> "\""
      _ <- recordPresenceAtAtom @branch (TickId (ftTickId matched)) path (Character charBranch) event
      pure ()
    [] -> warning $ "resolveAnchor: " <> T.pack path <> ": at_text \"" <> atText
                   <> "\" for " <> unBranchName charBranch <> " didn't match any atom, skipping"
    _  -> warning $ "resolveAnchor: " <> T.pack path <> ": at_text \"" <> atText
                   <> "\" for " <> unBranchName charBranch <> " matched more than one atom, skipping"
  where
    atoms = [ ft | ft <- ticks, ftContent ft /= Nothing ]

squashWhitespace :: T.Text -> T.Text
squashWhitespace = T.unwords . T.words

-- ---------------------------------------------------------------------------
-- Effectful wrapper: one file
-- ---------------------------------------------------------------------------

-- | Track presence for one scene file already committed on @branch@: reads
--   its current whole text, the story's full cast (via
--   'Storyteller.Core.ContentEffects.knownCast'), and who's already marked
--   present, hands all three to 'presenceAgent', then resolves and applies
--   every decision via 'resolveAnchor'. Safe to re-run: a decision that's
--   already reflected in the file's state as of its own resolved anchor is
--   a no-op there (see 'Storyteller.Writer.Presence.recordPresenceAtAtom's own
--   Haddock), so nothing here needs to track "have I already processed
--   this file" itself.
trackPresenceFor
  :: forall branch r
  .  ( LLMs r
     , Members '[ PromptStorage, StoryStorage, BranchOp branch, Cast
                , FileSystem (BranchTag branch), FileSystemRead (BranchTag branch)
                , Fail, Logging] r
     )
  => FilePath -> Sem r [PresenceDecision]
trackPresenceFor path = do
  exists <- fileExists @(BranchTag branch) path
  if not exists
    then [] <$ warning ("trackPresenceFor: " <> T.pack path <> " has no content yet, skipping")
    else do
      sceneText <- TE.decodeUtf8 <$> readFile @(BranchTag branch) path
      cast      <- knownCast
      present   <- map unCharacter <$> activeCharactersFor @branch path
      info $ "trackPresenceFor: " <> T.pack path <> ": " <> T.pack (show (length cast)) <> " known character(s)"
      decisions <- presenceAgent cast present sceneText
      ticks     <- runFileTicks @branch (fileTicksOf @branch path)
      mapM_ (resolveAnchor @branch path ticks) decisions
      return decisions

-- ---------------------------------------------------------------------------
-- Effectful wrapper: every chapter, in reading order
-- ---------------------------------------------------------------------------

-- | Run 'trackPresenceFor' over every recognized prose unit on @branch@, in
--   reading order (see 'Storyteller.Writer.Library.narrativeUnits') --
--   oldest first, so a character already marked present by an earlier
--   chapter is visible to 'presenceAgent' as context for a later one (see
--   the module Haddock). Skips a unit with no prose written yet
--   ('uiPath' is 'Nothing' for a beat-sheet-only unit) -- there's no text
--   for the model to read.
trackPresenceForAll
  :: forall branch r
  .  ( LLMs r
     , Members '[ PromptStorage, StoryStorage, BranchOp branch, Cast
                , FileSystem (BranchTag branch), FileSystemRead (BranchTag branch)
                , Fail, Logging] r
     )
  => Sem r [(FilePath, [PresenceDecision])]
trackPresenceForAll = do
  paths <- listAllFiles @(BranchTag branch) "/"
  let units = narrativeUnits (buildLibraryTree paths)
      scenePaths = mapMaybe uiPath units
  info $ "trackPresenceForAll: " <> T.pack (show (length scenePaths)) <> " chapter(s) to review"
  mapM (\p -> (,) p <$> trackPresenceFor @branch p) scenePaths
