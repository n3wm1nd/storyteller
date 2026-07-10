{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Checks for whether a model actually produced /well-formed/ tool calls,
--   not just whether the agent using them eventually got a usable result.
--   'Storyteller.Writer.Agent.Outline.splitOutlineAgent' already retries a
--   malformed @emit_beat_sheet@ call by feeding the parse error back to the
--   model (see its Haddock), so a broken run can still finish with
--   correct-looking 'Storyteller.Writer.Agent.Outline.ChapterBeats' at the
--   cost of extra turns the model shouldn't have needed. These checks make
--   that cost visible instead of it silently disappearing behind a
--   successful retry -- the project's testing philosophy is to write a test
--   the moment a hypothesis about behaviour needs checking, and "does this
--   model reliably format tool calls" is exactly that kind of hypothesis.
--   See 'Agent.Integration.Harness.recordToolCalls', which supplies the raw
--   per-turn data these operate on.
module Agent.Integration.ToolCallQuality
  ( TurnReport(..)
  , reportTurn
  , stringArguments
  , escapingArtifacts
  , invalidCallsSinceLastUser
  ) where

import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Aeson as Aeson
import qualified Autodocodec.Aeson.Compat as Compat

import UniversalLLM (Message(..), ToolCall(..))

-- | What one 'Runix.LLM.QueryLLM' response actually contained, boiled down
--   to the facts a tool-call reliability check cares about. Only
--   'AssistantTool' messages are relevant here -- plain text/reasoning in
--   the same turn is beside the point.
data TurnReport = TurnReport
  { trValidCalls   :: [(Text, Aeson.Value)]  -- ^ (tool name, parsed arguments) for calls that parsed
  , trInvalidCalls :: [(Text, Text)]         -- ^ (tool name, parse error) for calls that didn't
  } deriving (Show, Eq)

instance Semigroup TurnReport where
  TurnReport ok1 bad1 <> TurnReport ok2 bad2 = TurnReport (ok1 <> ok2) (bad1 <> bad2)

instance Monoid TurnReport where
  mempty = TurnReport [] []

-- | Classify one turn's response messages.
reportTurn :: [Message m] -> TurnReport
reportTurn = foldMap classify
  where
    classify (AssistantTool (ToolCall _ name args)) = TurnReport [(name, args)] []
    classify (AssistantTool (InvalidToolCall _ name _raw err)) = TurnReport [] [(name, err)]
    classify _ = mempty

-- | Every string leaf in a parsed tool call's arguments -- the values a
--   model actually typed into JSON string literals, after 'Data.Aeson'
--   decoding. This is deliberately the *decoded* Haskell 'Text', not the
--   raw wire JSON: a correctly-escaped @\n@ in the wire form has already
--   become a real newline character by the time it gets here, so anything
--   'escapingArtifacts' below flags is not just "contains an escape
--   sequence" but "still looks escaped after decoding," which only happens
--   when the model over-escaped in the first place.
stringArguments :: Aeson.Value -> [Text]
stringArguments (Aeson.String s) = [s]
stringArguments (Aeson.Object o) = concatMap stringArguments (map snd (Compat.toList o))
stringArguments (Aeson.Array a)  = concatMap stringArguments (toList a)
stringArguments _                = []

-- | Flag substrings that shouldn't survive a correct JSON encode/decode
--   round trip. A model that means to write a literal newline inside a
--   string argument emits the two-character escape @\n@, which decodes to
--   an actual newline -- so a literal backslash-n (or backslash-quote,
--   backslash-backslash) still present in the *decoded* text means the
--   model over-escaped (wrote @\\n@ for a newline, @\\\"@ for a quote) or
--   emitted a raw control character it should have escaped once, not zero
--   or two times. Heuristic, not a proof: prose could in principle contain
--   a literal backslash-n on purpose, but that's rare enough that a hit
--   here is worth a human look.
escapingArtifacts :: Text -> [Text]
escapingArtifacts t = concat
  [ ["literal \\n"  | "\\n"  `T.isInfixOf` t]
  , ["literal \\t"  | "\\t"  `T.isInfixOf` t]
  , ["literal \\\"" | "\\\"" `T.isInfixOf` t]
  , ["literal \\\\" | "\\\\" `T.isInfixOf` t]
  ]

-- | How many malformed tool calls the model has already made on the
--   *current* request -- i.e. within the suffix of @history@ after its last
--   user message, not counting retries from some earlier, already-resolved
--   request further back in the same conversation. No separate counter
--   needed for this: 'Runix.LLM.QueryLLM' is handed the full, growing
--   history on every turn (agent tool loops like
--   'Storyteller.Writer.Agent.Outline.splitOutlineAgent'\'s carry every
--   prior turn's 'ToolResultMsg' forward), so the transcript itself already
--   says how many times this request has been retried.
invalidCallsSinceLastUser :: [Message m] -> Int
invalidCallsSinceLastUser history =
  length [ () | AssistantTool (InvalidToolCall _ _ _ _) <- currentRequest ]
  where
    currentRequest = reverse (takeWhile (not . isUserMessage) (reverse history))
    isUserMessage (UserText _)         = True
    isUserMessage (UserRequestJSON _ _) = True
    isUserMessage _                    = False
