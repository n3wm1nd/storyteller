{-# LANGUAGE OverloadedStrings #-}

-- | Parses every worked example from @CONTEXT-DSL.md@ (the design spec)
--   verbatim, plus a handful of malformed inputs checked for a sensible
--   error location -- the two things this parser exists to get right:
--   accepting the concrete syntax the spec's own scenarios were
--   validated against, and failing predictably (line/column, not a
--   crash or a silent wrong parse) on the rest.
module Storyteller.Context.DSL.ParserSpec (spec) where

import Data.Text (Text)
import Test.Hspec

import Storyteller.Context.DSL.AST
import Storyteller.Context.DSL.Parser (ParseErr(..), parseDefinition)

shouldParse :: Text -> Expectation
shouldParse src =
  case parseDefinition "<test>" src of
    Left err -> expectationFailure
      ("expected a successful parse, got: line " <> show (peLine err)
        <> ", col " <> show (peCol err) <> ": " <> show (peMessage err))
    Right _ -> pure ()

shouldFailAt :: Text -> (Int, Int) -> Expectation
shouldFailAt src (line, col) =
  case parseDefinition "<test>" src of
    Left err -> (peLine err, peCol err) `shouldBe` (line, col)
    Right def -> expectationFailure
      ("expected a parse failure at " <> show (line, col) <> ", parsed instead: " <> show def)

spec :: Spec
spec = do
  workedExamplesSpec
  shapeSpec
  errorSpec

workedExamplesSpec :: Spec
workedExamplesSpec = describe "worked examples from CONTEXT-DSL.md" $ do
  it "parses the injury/status continuity example" $
    shouldParse
      "as \"injury\": read status/injury.md\n\
      \read \"injury\" | orifempty \"not injured\"\n"

  it "parses the invented-calendar example" $
    shouldParse
      "calendar_context = dateMath:\n\
      \  as \"rules\": read \"lore/calendar_system.md\"\n\
      \  dateMath (read \"calendar/log.md\" | latest(1))\n"

  it "parses the Chekhov's-gun list example" $
    shouldParse
      "as \"open\":\n\
      \  for f in tracking/**.md:\n\
      \    as f: read f\n\
      \\n\
      \\"open threads:\"\n\
      \for f in tracking/**.md:\n\
      \  f | filewithname\n"

  it "parses the voice-drift check example" $
    shouldParse
      "voice_check = charname:\n\
      \  as \"generator_context\":\n\
      \    in (charname | branch): read \"sheet.md\"\n\
      \    -- samples deliberately absent; the prose agent never sees them\n\
      \\n\
      \  as \"checker_context\":\n\
      \    \"Voice profile for %charname%:\"\n\
      \    in (charname | branch): read \"dialogue_samples.md\" | pinned\n"

  it "parses the relationship temperature example" $
    shouldParse
      "relationship_context = charA: charB:\n\
      \  \"%charA | charname% thinks of %charB | charname%:\"\n\
      \  in (charA | branch): read \"relationships/%charB%/temperature.md\"\n\
      \  -- repeat in the other direction\n"

  it "parses the personal prose-tic detector example" $
    shouldParse "read \"style/tics.md\"\n"

  it "parses the magic-system compliance example" $
    shouldParse
      "casting_status = charname:\n\
      \  in (charname | branch): read \"status/casting_log.md\" | orifempty \"no casting today\"\n\
      \\n\
      \magic_compliance_context = chapterPath:\n\
      \  as \"rules\": read \"lore/magic_system.md\"\n\
      \  as \"casting_history\":\n\
      \    for p in presence/%chapterPath%/*.md:\n\
      \      as p: casting_status (p | charname)\n"

  it "parses the living glossary example" $
    shouldParse
      "glossary_update = chapterPath:\n\
      \  known = read \"glossary/index.md\"\n\
      \  for term in mentions/%chapterPath%/**:\n\
      \    name = read term | extractProperNouns\n\
      \    if_new = name | exclude(known)\n\
      \    as name:\n\
      \      \"%name%: \"\n\
      \      name | draftDefinition(chapterPath)\n"

  it "parses the capture-before-narrowing example" $
    shouldParse
      "root = **/*\n\
      \for f in tracking/**.md:\n\
      \  in root: checkAgainstStyleGuide f\n"

shapeSpec :: Spec
shapeSpec = describe "AST shape" $ do
  it "an empty (or whitespace-only) source is a valid 0-arity, empty-body definition -- not a parse error" $ do
    parseDefinition "<test>" "" `shouldBe` Right (Definition [] [])
    parseDefinition "<test>" "\n  \n" `shouldBe` Right (Definition [] [])

  it "produces Assistant text for a bare '>' literal" $
    parseDefinition "<test>" "> \"seed text\"\n"
      `shouldBe` Right (Definition [] [Located (Pos 1 1) (SExpr (EAssistant (EString Quoted [Lit "seed text"])))])

  it "wraps a general expression (not just a literal) for '>'" $
    parseDefinition "<test>" "> read f\n"
      `shouldBe` Right (Definition [] [Located (Pos 1 1) (SExpr (EAssistant (ERead (PathLit Bare [Lit "f"]))))])

  it "wraps a general expression (not just a literal) for '<'" $
    parseDefinition "<test>" "< read notes.md\n"
      `shouldBe` Right (Definition []
        [ Located (Pos 1 1) (SExpr (EUser (ERead (PathLit Bare [Lit "notes.md"])))) ])

  it "distinguishes quoted (inert) from bare (glob) tokens" $
    parseDefinition "<test>" "\"**/*\"\nfor f in **/*:\n  f\n"
      `shouldBe` Right (Definition []
        [ Located (Pos 1 1) (SExpr (EString Quoted [Lit "**/*"]))
        , Located (Pos 2 1) (SFor "f" (EString Bare [Lit "**/*"]) [Located (Pos 3 3) (SExpr (EIdent "f"))])
        ])

  it "accepts any expression as a for-loop source, not just a literal glob -- a filtered call, here" $
    parseDefinition "<test>" "for f in (a | b):\n  f\n"
      `shouldBe` Right (Definition []
        [ Located (Pos 1 1) (SFor "f" (EFilter (EIdent "a") "b" []) [Located (Pos 2 3) (SExpr (EIdent "f"))]) ])

  it "splits %name% interpolation out of a bare path" $
    parseDefinition "<test>" "read presence/%chapterPath%/*.md\n"
      `shouldBe` Right (Definition []
        [ Located (Pos 1 1) (SExpr (ERead (PathLit Bare
            [Lit "presence/", Interp "chapterPath", Lit "/*.md"]))) ])

  it "parses a curried function head with a multi-statement body" $
    parseDefinition "<test>" "a: b:\n  x\n  y\n"
      `shouldBe` Right (Definition ["a", "b"]
        [ Located (Pos 2 3) (SExpr (EIdent "x"))
        , Located (Pos 3 3) (SExpr (EIdent "y"))
        ])

  it "parses filter chains with both bare and parenthesized arguments" $
    parseDefinition "<test>" "x | a | b(1, 2) | c d\n"
      `shouldBe` Right (Definition []
        [ Located (Pos 1 1) (SExpr
            (EFilter (EFilter (EFilter (EIdent "x") "a" [])
                               "b" [EIdent "1", EIdent "2"])
                     "c" [EIdent "d"])) ])

errorSpec :: Spec
errorSpec = describe "parse errors" $ do
  it "reports a specific location for a dangling 'as' with no body" $
    "as \"x\":\n" `shouldFailAt` (2, 1)

  it "reports a specific location for an under-indented nested block" $
    "in root:\n\
    \read foo\n"
      `shouldFailAt` (2, 1)

