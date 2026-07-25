{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Round-trip correctness for "Storyteller.Context.DSL.PrettyPrint":
--   parse -> print -> parse must yield an AST equal to the original for
--   every real definition this application ships, not just a hand-picked
--   toy example -- the whole point of the module is serving a compiled-in
--   library default's *actual* source, so a printer that's only correct
--   on simple cases would silently reintroduce the drift bug this
--   replaced (see the module's own Haddock, and the project chat that
--   settled "endpoint over hand-copied JS mirror").
module Storyteller.Context.DSL.PrettyPrintSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Storyteller.Context.DSL.AST
import Storyteller.Context.DSL.Library (defaultLibraryOrder)
import Storyteller.Context.DSL.Parser (parseDefinition)
import Storyteller.Context.DSL.PrettyPrint (prettyDefinition)

spec :: Spec
spec = describe "prettyDefinition" $ do
  describe "round-trips every compiled-in library default" $
    mapM_ roundTripsCase defaultLibraryOrder

  describe "small, targeted shapes" $ do
    it "a 0-arity string literal" $
      roundTrips (Definition [] [Located (Pos 1 1) (SExpr (EString Quoted [Lit "hello"]))])

    it "a bare glob, always printed bracketed" $
      roundTrips (Definition [] [Located (Pos 1 1) (SExpr (ERead (EString Bare [Lit "lore/**/*"])))])

    it "%name% interpolation in both quoted and bare positions" $
      roundTrips (Definition ["f"]
        [ Located (Pos 1 1) (SExpr (EString Quoted [Lit "## ", Interp "f"]))
        , Located (Pos 2 1) (SExpr (ERead (EString Bare [Lit "lore/", Interp "f", Lit ".md"])))
        ])

    it "for/as/in/let nesting" $
      roundTrips (Definition []
        [ Located (Pos 1 1) (SFor "f" (EString Bare [Lit "**/*"])
            [ Located (Pos 2 3) (SLet "x" Nothing [Located (Pos 2 7) (SExpr (EIdent "f"))])
            , Located (Pos 3 3) (SAs (EIdent "f") [Located (Pos 3 8) (SExpr (EIdent "x"))])
            ])
        ])

    it "a curried let binding" $
      roundTrips (Definition []
        [Located (Pos 1 1) (SLet "greet" (Just ["name"]) [Located (Pos 2 3) (SExpr (EIdent "name"))])])

    it "a filter chain with zero, one bare, one quoted, and multiple arguments" $
      roundTrips (Definition []
        [ Located (Pos 1 1) (SExpr (EFilter (EIdent "x") "sortBy" []))
        , Located (Pos 2 1) (SExpr (EFilter (EIdent "x") "orifempty" [EString Quoted [Lit "none"]]))
        , Located (Pos 3 1) (SExpr (EFilter (EIdent "x") "latest" [EIdent "n"]))
        , Located (Pos 4 1) (SExpr (EFilter (EIdent "x") "without" [EIdent "a", EIdent "b"]))
        ])

    it "a non-atomic application argument needs parens to round-trip" $
      roundTrips (Definition []
        [Located (Pos 1 1) (SExpr (EApp (EIdent "loreEntry") [EFilter (EIdent "f") "orifempty" [EString Quoted [Lit "none"]]]))])

    it "a non-atomic single filter argument needs parens to round-trip" $
      roundTrips (Definition []
        [Located (Pos 1 1) (SExpr (EFilter (EIdent "x") "exclude" [EApp (EIdent "loreEntry") [EIdent "f"]]))])

    it "> and < wrapping a read" $
      roundTrips (Definition []
        [ Located (Pos 1 1) (SExpr (EAssistant (ERead (EString Bare [Lit "a.md"]))))
        , Located (Pos 2 1) (SExpr (EUser (ERead (EString Bare [Lit "b.md"]))))
        ])
  where
    roundTripsCase (name, def) = it (T.unpack name) $ roundTrips def

-- | Round-trip correctness only ever means "the same tree, modulo where
--   each statement started" -- 'Pos' is diagnostic (see 'AST.Located's
--   own Haddock: "the only place this is needed downstream is...
--   reporting *where*"), not semantic, and pretty-printed source
--   legitimately reflows onto different lines\/columns than whatever a
--   human originally wrote. Comparing 'Pos'-erased trees is therefore the
--   actual property this module promises, not full structural 'Eq'.
roundTrips :: Definition -> Expectation
roundTrips def = case parseDefinition "<pretty-printed>" (prettyDefinition def) of
  Left err       -> expectationFailure ("pretty-printed source failed to re-parse: " <> show err)
  Right reparsed -> eraseDefPos reparsed `shouldBe` eraseDefPos def

eraseDefPos :: Definition -> Definition
eraseDefPos (Definition params body) = Definition params (eraseBlockPos body)

eraseBlockPos :: Block -> Block
eraseBlockPos = map eraseLocPos

eraseLocPos :: Located Stmt -> Located Stmt
eraseLocPos (Located _ stmt) = Located (Pos 0 0) (eraseStmtPos stmt)

eraseStmtPos :: Stmt -> Stmt
eraseStmtPos = \case
  SExpr e        -> SExpr e
  SAs nameE body -> SAs nameE (eraseBlockPos body)
  SLet n ps body -> SLet n ps (eraseBlockPos body)
  SIn e body     -> SIn e (eraseBlockPos body)
  SFor v e body  -> SFor v e (eraseBlockPos body)
