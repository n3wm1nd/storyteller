{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Storyteller.Writer.WriteSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import UniversalLLM (Message(..))
import Storage.Tick (FileTick(..))

import Storyteller.Core.LLM.Role (ProseModel)
import qualified Storyteller.Context.DSL.Value as DSL
import Storyteller.Writer.Agent (Instruction(..), CharLabel(..), CharSummary(..))
import Storyteller.Writer.Agent.Write (buildChapterMessages)

-- | A bare-minimum atom tick: only the fields 'Storyteller.Writer.Agent.
--   Chat.historyFromFileTicks' actually reads (@ftKind@, @ftMessage@,
--   @ftContent@) are meaningful here.
atomTick :: T.Text -> FileTick
atomTick msg = FileTick
  { ftTickId = "atom", ftKind = "atom", ftRefs = [], ftFields = []
  , ftMessage = msg, ftContent = Just msg, ftParent = Nothing
  }

promptTick :: T.Text -> FileTick
promptTick msg = FileTick
  { ftTickId = "prompt", ftKind = "prompt", ftRefs = [], ftFields = []
  , ftMessage = msg, ftContent = Nothing, ftParent = Nothing
  }

noSummary :: CharSummary
noSummary = CharSummary [] [] []

build
  :: [Message ProseModel] -> [(CharLabel, CharSummary)] -> [(CharLabel, DSL.Message)] -> [DSL.Message] -> [FileTick] -> Instruction
  -> [Message ProseModel]
build = buildChapterMessages

-- | A single lore file, as the one @UserText@ message it'd arrive as once
--   flattened by whatever assembled the caller's own context (in
--   production, 'Storyteller.Context.DSL.Library.loreEntry''s own
--   default) -- kept local to this test so the existing cases below barely
--   change.
loreMsg :: T.Text -> Message ProseModel
loreMsg = UserText

-- | The pairing 'buildChapterMessages' used to construct itself from
--   @(path, content)@ pairs -- now built by the caller (in production, a
--   Context DSL definition's own @> read f@, see
--   'Storyteller.Context.DSL.Library.contextChapters') and just spliced
--   through. Kept local to this test so the existing cases below barely
--   change.
earlierChapterMsgs :: FilePath -> T.Text -> [Message ProseModel]
earlierChapterMsgs path content = [UserText ("## Chapter: " <> T.pack path), AssistantText content]

spec :: Spec
spec = describe "buildChapterMessages" $ do

  it "with nothing else gathered, is just the raw instruction, unwrapped" $ do
    build [] [] [] [] [] (Instruction "continue the scene")
      `shouldBe` [UserText "continue the scene"]

  it "puts world lore first, ahead of everything else" $ do
    let msgs = build [loreMsg "### notes/tavern.md\n\nA tavern."] [] [] [] [] (Instruction "go")
    case msgs of
      (m : _) -> m `shouldBe` UserText "### notes/tavern.md\n\nA tavern."
      []      -> expectationFailure "expected at least one message"

  it "puts earlier chapters right after world lore, oldest first, each as a naming user message plus its prose as an assistant message" $ do
    let msgs = build (earlierChapterMsgs "chapters/ch1.md" "chapter one prose" ++ earlierChapterMsgs "chapters/ch2.md" "chapter two prose") [] [] [] [] (Instruction "go")
    take 4 msgs `shouldBe`
      [ UserText "## Chapter: chapters/ch1.md", AssistantText "chapter one prose"
      , UserText "## Chapter: chapters/ch2.md", AssistantText "chapter two prose"
      ]

  -- Each block is its own message now, rather than one joined string: the
  -- boundaries are what a provider caches on, and a block that arrived as
  -- 'DSL.Assistant' keeps that role. What still has to hold is that the
  -- journal is *not* among them -- it belongs in the splice, not the
  -- stable prefix.
  it "puts a character's sheet/context at chapter start as separate messages, journal excluded from them" $ do
    let alice = CharSummary
          { csSheet   = [DSL.User "### sheet.md\n\n# Alice"]
          , csContext = [DSL.User "### notes.md\n\nsome context"]
          , csJournal = [DSL.User "### journal excerpt\n\nsecret diary entry"]
          }
        msgs = build [] [(CharLabel "Alice", alice)] [] [] [] (Instruction "go")
    take 3 msgs `shouldBe`
      [ UserText "## Character: Alice"
      , UserText "### sheet.md\n\n# Alice"
      , UserText "### notes.md\n\nsome context"
      ]
    -- the journal is elsewhere entirely, not folded into the prefix
    take 3 msgs `shouldSatisfy`
      all (\m -> not ("secret diary entry" `isInfixOfText` messageBody m))

  it "reconstructs the current chapter's own history as alternating turns, in order" $ do
    let ticks = [promptTick "write the opening", atomTick "Once upon a time...", promptTick "now the twist", atomTick "...and then everything changed."]
        msgs  = build [] [] [] [] ticks (Instruction "go")
    take 4 msgs `shouldBe`
      [ UserText "write the opening"
      , AssistantText "Once upon a time..."
      , UserText "now the twist"
      , AssistantText "...and then everything changed."
      ]

  it "places the journal excerpt in its own shallow splice message, directly before the instruction -- not inside it, not at chapter-start" $ do
    let alice = CharSummary { csSheet = [], csContext = [], csJournal = [DSL.User "### journal\n\nshe remembers the storm"] }
        msgs  = build [] [(CharLabel "Alice", alice)] [] [] [] (Instruction "continue")
    msgs `shouldBe`
      [ UserText "## Character: Alice"
      , UserText "### journal\n\nshe remembers the storm"
      , AssistantText "Noted."
      , UserText "continue"
      ]

  -- Pinned content and journals share the splice -- same position, adjacent
  -- -- but each is its own bracketed block (own message, own trailing
  -- 'AssistantText' ack), not joined into one string or one shared bracket:
  -- a pinned item the user just changed shouldn't reshape the journal
  -- block sitting next to it, and neither may sit directly against the
  -- other's own boundary without a turn marker between them.
  it "puts pinned context and journal excerpts adjacent in the splice, pinned first, each its own bracketed block" $ do
    let alice = CharSummary { csSheet = [], csContext = [], csJournal = [DSL.User "### journal\n\nshe remembers the storm"] }
        msgs  = build [] [(CharLabel "Alice", alice)] [] [DSL.User "### pinned.md\n\nuser-pinned note"] [] (Instruction "continue")
    msgs `shouldBe`
      [ UserText "### pinned.md\n\nuser-pinned note"
      , AssistantText "Noted."
      , UserText "## Character: Alice"
      , UserText "### journal\n\nshe remembers the storm"
      , AssistantText "Noted."
      , UserText "continue"
      ]

  -- A character's tasks are a separate read from journal (see
  -- 'Storyteller.Writer.Agent.Write.tasksForActiveCharacters' -- no
  -- override surface, so it's plumbed straight from the character's own
  -- branch rather than through 'CharSummary'), so it lands as its own
  -- bracketed splice entry, right after the journal's, not folded into it.
  it "puts a character's tasks in their own bracketed splice entry, right after the journal's" $ do
    let alice = CharSummary { csSheet = [], csContext = [], csJournal = [DSL.User "### journal\n\nshe remembers the storm"] }
        msgs  = build [] [(CharLabel "Alice", alice)] [(CharLabel "Alice", DSL.User "### tasks.md\n\n- [ ] find the sword")] [] [] (Instruction "continue")
    msgs `shouldBe`
      [ UserText "## Character: Alice"
      , UserText "### journal\n\nshe remembers the storm"
      , AssistantText "Noted."
      , UserText "## Character: Alice"
      , UserText "### tasks.md\n\n- [ ] find the sword"
      , AssistantText "Noted."
      , UserText "continue"
      ]

  -- The bug this section closes: the splice's own trailing message is
  -- always 'User'-roled content, and so is whatever follows it -- the
  -- final instruction, when the splice lands at the tail. Without a real
  -- turn boundary between them, a provider has nothing to key a split on
  -- and folds the splice into the instruction, silently blending "ambient
  -- journal context" into "what was actually asked".
  it "never lets the splice's own last message sit directly against the final instruction" $ do
    let alice = CharSummary { csSheet = [], csContext = [], csJournal = [DSL.User "### journal\n\nnote"] }
        msgs  = build [] [(CharLabel "Alice", alice)] [] [] [] (Instruction "continue")
    case reverse msgs of
      (UserText "continue" : second : _) -> second `shouldBe` AssistantText "Noted."
      other -> expectationFailure ("expected the instruction preceded by an Assistant ack, got " <> show (reverse other))

  it "the instruction is always the last message, regardless of what else is present, and is the raw prompt verbatim" $ do
    let alice = CharSummary
          { csSheet = [DSL.User "### sheet.md\n\n# Alice"], csContext = [], csJournal = [DSL.User "### journal\n\nnote"] }
        msgs = build
          ([loreMsg "### lore.md\n\nlore"] ++ earlierChapterMsgs "chapters/ch1.md" "earlier chapter")
          [(CharLabel "Alice", alice)]
          []
          [DSL.User "### pinned.md\n\npinned"]
          [atomTick "existing prose"]
          (Instruction "finish the scene")
    case reverse msgs of
      (UserText t : _) -> t `shouldBe` "finish the scene"
      other             -> expectationFailure ("expected the instruction as the last message, got " <> show (reverse other))

  it "drops empty sections instead of emitting empty messages" $ do
    build [] [(CharLabel "Alice", noSummary)] [] [] [] (Instruction "go")
      `shouldBe` [UserText "go"]

  it "with no splice, the instruction is exactly the raw prompt appended after full history -- no split point introduced for nothing" $ do
    let ticks = [promptTick "write the opening", atomTick "Once upon a time..."]
        msgs  = build [] [] [] [] ticks (Instruction "continue")
    msgs `shouldBe`
      [ UserText "write the opening"
      , AssistantText "Once upon a time..."
      , UserText "continue"
      ]

  it "keeps the splice within recentWindowMin/recentWindowMax turns of the end, not at the very front" $ do
    -- five completed turns, well past the window -- the splice must not be
    -- the very first message: some amount of older conversation should
    -- still lead it.
    let alice = CharSummary { csSheet = [], csContext = [], csJournal = [DSL.User "### journal\n\nnote"] }
        ticks = concat
          [ [promptTick ("prompt " <> T.pack (show n)), atomTick ("reply " <> T.pack (show n))]
          | n <- [1 :: Int .. 5]
          ]
        msgs = build [] [(CharLabel "Alice", alice)] [] [] ticks (Instruction "continue")
        -- the splice's first message; with csSheet/csContext empty there
        -- is no identity prefix, so this header appears only in the splice
        splice = UserText "## Character: Alice"
    case break (== splice) msgs of
      (before, _ : after) -> do
        before `shouldNotBe` []
        after `shouldNotBe` []
      (_, []) -> expectationFailure ("splice message not found in " <> show msgs)

  it "keeps at least one real conversation turn after the splice -- the model's lead-in to generating is the scene, not the context dump" $ do
    let alice = CharSummary { csSheet = [], csContext = [], csJournal = [DSL.User "### journal\n\nnote"] }
        ticks = concat
          [ [promptTick ("prompt " <> T.pack (show n)), atomTick ("reply " <> T.pack (show n))]
          | n <- [1 :: Int .. 5]
          ]
        msgs = build [] [(CharLabel "Alice", alice)] [] [] ticks (Instruction "continue")
        -- the splice's first message; with csSheet/csContext empty there
        -- is no identity prefix, so this header appears only in the splice
        splice = UserText "## Character: Alice"
    case break (== splice) msgs of
      (_, _ : after) -> init after `shouldSatisfy` any isConversationTurn
      (_, [])        -> expectationFailure ("splice message not found in " <> show msgs)

  it "holds the split boundary still across a whole window stretch, moving only once the recent side would exceed the max" $ do
    -- With min=2/max=4, 2..4 completed turns should all put the *entire*
    -- history on the recent side (boundary never moves within that
    -- stretch); a 5th turn is what finally pushes the boundary forward.
    let alice = CharSummary { csSheet = [], csContext = [], csJournal = [DSL.User "### journal\n\nnote"] }
        ticksThrough n = concat
          [ [promptTick ("prompt " <> T.pack (show i)), atomTick ("reply " <> T.pack (show i))]
          | i <- [1 .. n]
          ]
        -- the splice's first message; with csSheet/csContext empty there
        -- is no identity prefix, so this header appears only in the splice
        splice = UserText "## Character: Alice"
        olderCount n = length (fst (break (== splice) (build [] [(CharLabel "Alice", alice)] [] [] (ticksThrough n) (Instruction "go"))))
    mapM_ (\n -> olderCount n `shouldBe` 0) [2, 3, 4 :: Int]
    olderCount 5 `shouldSatisfy` (> 0)

  -- The actual property the whole window mechanism exists for: as long as
  -- consecutive turns fall in the same window stretch, one turn's full
  -- sent-request-plus-response is a byte-identical *prefix* of the next
  -- turn's request -- the shape a provider's prefix cache can serve for
  -- free, without needing an explicit cache breakpoint anywhere in this
  -- code. This is what the old design (prompt duplicated via both tick
  -- history and a wrapped instruction message, plus a splice interposed
  -- fresh every turn) broke on literally every turn; regressing to that
  -- shape should fail this test immediately.
  it "gives a byte-identical prefix across consecutive turns within the same window stretch" $ do
    let alice = CharSummary { csSheet = [], csContext = [], csJournal = [DSL.User "### journal\n\nnote"] }
        ticksThrough n = concat
          [ [promptTick ("prompt " <> T.pack (show i)), atomTick ("reply " <> T.pack (show i))]
          | i <- [1 .. n]
          ]
        requestForTurn n = build [] [(CharLabel "Alice", alice)] [] []
          (ticksThrough n) (Instruction ("prompt " <> T.pack (show (n + 1))))
        -- everything sent to generate turn 3, plus the reply it got back
        merged  = requestForTurn 2 ++ [AssistantText "reply 3"]
        -- what actually gets sent to generate turn 4
        nextReq = requestForTurn 3
    merged `shouldBe` take (length merged) nextReq
  where
    isInfixOfText needle haystack = needle `T.isInfixOf` haystack
    isConversationTurn (AssistantText _) = True
    isConversationTurn _                 = False

-- | The text of a message whatever its role -- for assertions about what
--   did or didn't reach a given position, independent of framing.
messageBody :: Message ProseModel -> T.Text
messageBody (UserText t)      = t
messageBody (AssistantText t) = t
messageBody _                 = ""
