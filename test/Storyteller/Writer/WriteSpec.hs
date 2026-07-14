{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Storyteller.Writer.WriteSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import UniversalLLM (Message(..))
import Storage.Tick (FileTick(..))

import Storyteller.Core.LLM.Role (ProseModel)
import Storyteller.Writer.Agent (Instruction(..), ContextBlock(..), CharContextBlock(..), CharLabel(..), CharSummary(..))
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
  :: [ContextBlock] -> [(CharLabel, CharSummary)] -> [ContextBlock] -> [(FilePath, T.Text)] -> [FileTick] -> Instruction
  -> [Message ProseModel]
build = buildChapterMessages

spec :: Spec
spec = describe "buildChapterMessages" $ do

  it "with nothing else gathered, is just the raw instruction, unwrapped" $ do
    build [] [] [] [] [] (Instruction "continue the scene")
      `shouldBe` [UserText "continue the scene"]

  it "puts world lore first, ahead of everything else" $ do
    let msgs = build [ContextBlock "### notes/tavern.md\n\nA tavern."] [] [] [] [] (Instruction "go")
    case msgs of
      (m : _) -> m `shouldBe` UserText "### notes/tavern.md\n\nA tavern."
      []      -> expectationFailure "expected at least one message"

  it "puts earlier chapters right after world lore, oldest first, each as a naming user message plus its prose as an assistant message" $ do
    let msgs = build [] [] [] [("chapters/ch1.md", "chapter one prose"), ("chapters/ch2.md", "chapter two prose")] [] (Instruction "go")
    take 4 msgs `shouldBe`
      [ UserText "## Chapter: chapters/ch1.md", AssistantText "chapter one prose"
      , UserText "## Chapter: chapters/ch2.md", AssistantText "chapter two prose"
      ]

  it "puts a character's sheet/context as one chapter-start message, journal excluded from it" $ do
    let alice = CharSummary
          { csSheet   = [CharContextBlock "### sheet.md\n\n# Alice"]
          , csContext = [CharContextBlock "### notes.md\n\nsome context"]
          , csJournal = [CharContextBlock "### journal excerpt\n\nsecret diary entry"]
          }
        msgs = build [] [(CharLabel "Alice", alice)] [] [] [] (Instruction "go")
    case msgs of
      (UserText t : _) -> do
        t `shouldSatisfy` (\txt -> "## Character: Alice" `isInfixOfText` txt)
        t `shouldSatisfy` (\txt -> "# Alice" `isInfixOfText` txt)
        t `shouldSatisfy` (\txt -> "some context" `isInfixOfText` txt)
        t `shouldSatisfy` (\txt -> not ("secret diary entry" `isInfixOfText` txt))
      other -> expectationFailure ("expected a UserText chapter-start message first, got " <> show other)

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
    let alice = CharSummary { csSheet = [], csContext = [], csJournal = [CharContextBlock "### journal\n\nshe remembers the storm"] }
        msgs  = build [] [(CharLabel "Alice", alice)] [] [] [] (Instruction "continue")
    msgs `shouldBe`
      [ UserText "## Character: Alice\n\n### journal\n\nshe remembers the storm"
      , UserText "continue"
      ]

  it "merges pinned context and journal excerpts into the same single splice message" $ do
    let alice = CharSummary { csSheet = [], csContext = [], csJournal = [CharContextBlock "### journal\n\nshe remembers the storm"] }
        msgs  = build [] [(CharLabel "Alice", alice)] [ContextBlock "### pinned.md\n\nuser-pinned note"] [] [] (Instruction "continue")
    length msgs `shouldBe` 2
    case msgs of
      (UserText t : _) -> do
        t `shouldSatisfy` (\txt -> "user-pinned note" `isInfixOfText` txt)
        t `shouldSatisfy` (\txt -> "she remembers the storm" `isInfixOfText` txt)
      other -> expectationFailure ("expected the merged splice message first, got " <> show other)

  it "the instruction is always the last message, regardless of what else is present, and is the raw prompt verbatim" $ do
    let alice = CharSummary
          { csSheet = [CharContextBlock "### sheet.md\n\n# Alice"], csContext = [], csJournal = [CharContextBlock "### journal\n\nnote"] }
        msgs = build
          [ContextBlock "### lore.md\n\nlore"]
          [(CharLabel "Alice", alice)]
          [ContextBlock "### pinned.md\n\npinned"]
          [("chapters/ch1.md", "earlier chapter")]
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
    let alice = CharSummary { csSheet = [], csContext = [], csJournal = [CharContextBlock "### journal\n\nnote"] }
        ticks = concat
          [ [promptTick ("prompt " <> T.pack (show n)), atomTick ("reply " <> T.pack (show n))]
          | n <- [1 :: Int .. 5]
          ]
        msgs = build [] [(CharLabel "Alice", alice)] [] [] ticks (Instruction "continue")
        splice = UserText "## Character: Alice\n\n### journal\n\nnote"
    case break (== splice) msgs of
      (before, _ : after) -> do
        before `shouldNotBe` []
        after `shouldNotBe` []
      (_, []) -> expectationFailure ("splice message not found in " <> show msgs)

  it "keeps at least one real conversation turn after the splice -- the model's lead-in to generating is the scene, not the context dump" $ do
    let alice = CharSummary { csSheet = [], csContext = [], csJournal = [CharContextBlock "### journal\n\nnote"] }
        ticks = concat
          [ [promptTick ("prompt " <> T.pack (show n)), atomTick ("reply " <> T.pack (show n))]
          | n <- [1 :: Int .. 5]
          ]
        msgs = build [] [(CharLabel "Alice", alice)] [] [] ticks (Instruction "continue")
        splice = UserText "## Character: Alice\n\n### journal\n\nnote"
    case break (== splice) msgs of
      (_, _ : after) -> init after `shouldSatisfy` any isConversationTurn
      (_, [])        -> expectationFailure ("splice message not found in " <> show msgs)

  it "holds the split boundary still across a whole window stretch, moving only once the recent side would exceed the max" $ do
    -- With min=2/max=4, 2..4 completed turns should all put the *entire*
    -- history on the recent side (boundary never moves within that
    -- stretch); a 5th turn is what finally pushes the boundary forward.
    let alice = CharSummary { csSheet = [], csContext = [], csJournal = [CharContextBlock "### journal\n\nnote"] }
        ticksThrough n = concat
          [ [promptTick ("prompt " <> T.pack (show i)), atomTick ("reply " <> T.pack (show i))]
          | i <- [1 .. n]
          ]
        splice = UserText "## Character: Alice\n\n### journal\n\nnote"
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
    let alice = CharSummary { csSheet = [], csContext = [], csJournal = [CharContextBlock "### journal\n\nnote"] }
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
