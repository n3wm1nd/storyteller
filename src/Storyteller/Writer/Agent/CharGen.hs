{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

-- | Character generation agent.
--
--   Given a scenario template (parsed YAML) and a seed, resolves random
--   selections and numeric rolls into a character sheet.
--
--   'charGenAgent' is pure — its result is 'CharSheet', plain 'Text', not a
--   file. Whether and where it gets written is the caller's business: see
--   'Storyteller.Core.Storage.store' at the call site.
module Storyteller.Writer.Agent.CharGen
  ( charGenAgent
  , drawSeed
  , ScenarioTemplate(..)
  , RngSeed(..)
  , CharSheet(..)
  , unSheet
  ) where

import           Control.Monad (replicateM, when)
import           Control.Monad.State.Strict
import           Data.Aeson (Value(..))
import qualified Data.Aeson.Key    as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Scientific (Scientific, toBoundedInteger)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import           System.Random (StdGen, mkStdGen, uniformR)
import           Text.Read (readMaybe)

import Polysemy
import Runix.Random (Random, randomInt)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

newtype ScenarioTemplate = ScenarioTemplate { unTemplate :: Value }
newtype RngSeed          = RngSeed          { unSeed     :: Int  }
newtype CharSheet        = CharSheet        { unSheet    :: Text }

-- | Draw a fresh seed when the caller doesn't already have one to reuse.
drawSeed :: Member Random r => Sem r RngSeed
drawSeed = RngSeed <$> randomInt

charGenAgent :: ScenarioTemplate -> RngSeed -> CharSheet
charGenAgent (ScenarioTemplate raw) (RngSeed seed) =
  let cats    = categories raw
      initSh  = Map.map (const (RMap Map.empty)) (Map.filter (not . hasKey "value") cats)
      resolved = execState (resolveAll cats raw) (initSh, mkStdGen seed)
  in  CharSheet (renderSheet raw (fst resolved))

-- ---------------------------------------------------------------------------
-- Schema types
-- ---------------------------------------------------------------------------

-- | The kind of a top-level category.
data CatKind = Numeric | Selection deriving (Eq)

catKind :: Value -> Maybe CatKind
catKind v
  | hasKey "value" v = Just Numeric
  | hasKey "roll"  v = Just Selection
  | otherwise        = Nothing

-- | A reference from a granted entry to another category.
data Ref
  = NumericRef (Map Text Int)   -- ^ delta map: {entry_id -> delta}
  | SelectRef  SelectSpec

-- | How to select from a category.
data SelectSpec = SelectSpec
  { specGrants  :: [Text]
  , specRoll    :: Maybe Int    -- ^ how many to roll randomly
  , specChoices :: Maybe [Text] -- ^ restrict pool to these ids
  }

-- ---------------------------------------------------------------------------
-- Resolution result
-- ---------------------------------------------------------------------------

data Resolved
  = RInt Int
  | RMap (Map Text Resolved)
  | RPresent              -- ^ selected entry with no sub-data
  deriving (Show)

type Sheet = Map Text Resolved

-- | Our resolution monad: mutable sheet + RNG.
type Resolve a = State (Sheet, StdGen) a

-- ---------------------------------------------------------------------------
-- Resolution
-- ---------------------------------------------------------------------------

resolveAll :: Map Text Value -> Value -> Resolve ()
resolveAll cats template = mapM_ (uncurry resolveCategory) (Map.toList (categories template))
  where
    resolveCategory name def = case catKind def of
      Just Numeric   -> resolveNumeric name def
      Just Selection -> resolveSelection cats name def (parseSelectSpec def)
      Nothing        -> return ()

-- | Roll dice for each stat and store in the sheet.
resolveNumeric :: Text -> Value -> Resolve ()
resolveNumeric name def = do
  r <- case entryIds def of
    [] -> RInt <$> roll (getStr "value" def)
    ids | getStr "assign" def == "free" -> do
            rolls  <- replicateM (length ids) (roll (getStr "value" def))
            order  <- shuffled ids
            return . RMap . Map.fromList $ zip order (map RInt (List.sortBy (flip compare) rolls))
        | otherwise ->
            RMap . Map.fromList <$> mapM (\i -> fmap (\n -> (i, RInt n)) (roll (getStr "value" def))) ids
  modify' $ \(sh, g) -> (Map.insert name r sh, g)

-- | Grant selected entries and apply their refs.
resolveSelection :: Map Text Value -> Text -> Value -> SelectSpec -> Resolve ()
resolveSelection cats name def spec = do
  mapM_ grant (specGrants spec)
  case specRoll spec of
    Nothing -> return ()
    Just n  -> do
      already <- selectedIn name
      let pool  = fromMaybe (entryIds def) (specChoices spec)
          avail = filter (`notElem` already) pool
      chosen <- sampleN n avail
      mapM_ grant chosen
  where
    grant eid = do
      already <- selectedIn name
      when (eid `notElem` already) $ do
        modify' $ \(sh, g) -> (Map.adjust (insertEntry eid RPresent) name sh, g)
        mapM_ applyRef (parseRefs (lookupEntry eid def))

    applyRef (refName, NumericRef deltas) =
      modify' $ \(sh, g) -> (Map.adjust (applyDeltas deltas) refName sh, g)
    applyRef (refName, SelectRef refSpec) =
      case Map.lookup refName cats of
        Nothing     -> return ()
        Just refDef -> resolveSelection cats refName refDef refSpec

selectedIn :: Text -> Resolve [Text]
selectedIn name = do
  sh <- gets fst
  return $ case Map.lookup name sh of
    Just (RMap m) -> Map.keys m
    _             -> []

insertEntry :: Text -> Resolved -> Resolved -> Resolved
insertEntry k v (RMap m) = RMap (Map.insert k v m)
insertEntry _ _ r        = r

applyDeltas :: Map Text Int -> Resolved -> Resolved
applyDeltas deltas (RMap m) =
  RMap $ Map.foldlWithKey' (\acc k d -> Map.adjust (addInt d) k acc) m deltas
applyDeltas _ r = r

addInt :: Int -> Resolved -> Resolved
addInt d (RInt n) = RInt (n + d)
addInt _ r        = r

-- ---------------------------------------------------------------------------
-- Parsing refs and specs from YAML
-- ---------------------------------------------------------------------------

parseSelectSpec :: Value -> SelectSpec
parseSelectSpec v = SelectSpec
  { specGrants  = maybe [] valueToTexts (lookupKey "grant" v)
  , specRoll    = fmap (floor . toRealFloat) (lookupKey "roll" v >>= toNumber)
  , specChoices = fmap valueToTexts (lookupKey "choices" v)
  }
  where
    toNumber (Number n) = Just n
    toNumber _          = Nothing
    toRealFloat         = realToFrac :: Scientific -> Double

parseRefs :: Value -> [(Text, Ref)]
parseRefs v = [ (k, toRef k val)
              | (k, val) <- objPairs v
              , k `notElem` reserved ]
  where
    toRef _ (Object o)
      | all isNumericDelta (KM.elems o) =
          NumericRef (Map.fromList [ (K.toText k, toDelta v')
                                   | (k, v') <- KM.toList o ])
    toRef _ (Array arr) = SelectRef SelectSpec
      { specGrants  = valueToTexts (Array arr)
      , specRoll    = Nothing
      , specChoices = Nothing }
    toRef _ _ = SelectRef SelectSpec
      { specGrants  = []
      , specRoll    = Just 1
      , specChoices = Nothing }

    isNumericDelta (Number _) = True
    isNumericDelta (String s) = case T.unpack (T.strip s) of
                                  (c:_) -> c `elem` ['+','-'] || (c >= '0' && c <= '9')
                                  []    -> False
    isNumericDelta _          = False

    toDelta (Number n) = fromMaybe 0 (toBoundedInteger n)
    toDelta (String s) = fromMaybe 0 (readMaybe (T.unpack (T.strip s)))
    toDelta _          = 0

-- ---------------------------------------------------------------------------
-- Rendering
-- ---------------------------------------------------------------------------

lineW :: Int
lineW = 72

renderSheet :: Value -> Sheet -> Text
renderSheet template sheet =
  let cats  = categories template
      title = getStr "name" template
      desc  = getStr "description" template
      bar c = T.replicate lineW (T.singleton c)
      hdr   = T.unlines [ bar '=', "  " <> T.toUpper title
                         , if T.null desc then "" else "  " <> T.strip desc
                         , bar '=' ]
      body  = mconcat [ renderCategory k (cats Map.! k) r
                       | k <- Map.keys cats
                       , Just r <- [Map.lookup k sheet] ]
  in  hdr <> body <> "\n" <> bar '='

renderCategory :: Text -> Value -> Resolved -> Text
renderCategory name def resolved =
  let title = catTitle name def
      bar   = T.replicate lineW "-"
  in  "\n  " <> title <> "\n" <> bar <> "\n" <> renderResolved def resolved 4

renderResolved :: Value -> Resolved -> Int -> Text
renderResolved def (RMap m) indent
  | all isRInt (Map.elems m) = renderStatBlock (Map.toAscList m) def indent
  | otherwise = mconcat [ renderEntry eid v def indent | (eid, v) <- Map.toAscList m ]
renderResolved _ (RInt n) indent = T.replicate indent " " <> T.pack (show n) <> "\n"
renderResolved _ RPresent _      = ""

renderStatBlock :: [(Text, Resolved)] -> Value -> Int -> Text
renderStatBlock items def indent =
  let isFree = getStr "assign" def == "free"
      fmt (eid, RInt n) =
        let name  = entryName eid (lookupEntry eid def)
            score = if isFree
                    then let m    = (n - 10) `div` 2
                             sign = if m >= 0 then "+" else ""
                         in  T.pack (show n) <> "  (" <> sign <> T.pack (show m) <> ")"
                    else T.pack (show n)
        in  T.justifyLeft 18 ' ' name <> " " <> score
      fmt _ = ""
      rows  = map fmt items
      half  = (length rows + 1) `div` 2
      pad   = T.replicate indent " "
      cols l r = pad <> T.justifyLeft 32 ' ' l <> "  " <> r
  in  T.unlines $ zipWith cols (take half rows) (drop half rows ++ repeat "")

renderEntry :: Text -> Resolved -> Value -> Int -> Text
renderEntry eid val def indent =
  let entDef    = lookupEntry eid def
      name      = entryName eid entDef
      desc      = T.strip (getStr "description" entDef)
      pad       = T.replicate indent " "
      descSuffix = if T.null desc then "" else ": " <> T.take 80 desc
  in  case val of
        RInt  n -> pad <> name <> ": " <> T.pack (show n) <> "\n"
        RPresent -> pad <> name <> descSuffix <> "\n"
        RMap  m -> pad <> name <> descSuffix <> "\n"
                   <> mconcat [ renderEntry k v entDef (indent + 2) | (k, v) <- Map.toAscList m ]

-- ---------------------------------------------------------------------------
-- Schema helpers
-- ---------------------------------------------------------------------------

reserved :: [Text]
reserved = ["value","roll","assign","choices","grant","pick","name","description"]

categories :: Value -> Map Text Value
categories (Object o) =
  Map.fromList [ (K.toText k, v) | (k, v) <- KM.toList o
               , K.toText k `notElem` ["name","description"], isObj v ]
categories _ = Map.empty

isObj :: Value -> Bool
isObj (Object _) = True
isObj _          = False

isRInt :: Resolved -> Bool
isRInt (RInt _) = True
isRInt _        = False

objPairs :: Value -> [(Text, Value)]
objPairs (Object o) = [ (K.toText k, v) | (k, v) <- KM.toList o ]
objPairs _          = []

lookupKey :: Text -> Value -> Maybe Value
lookupKey k (Object o) = KM.lookup (K.fromText k) o
lookupKey _ _          = Nothing

getStr :: Text -> Value -> Text
getStr k v = case lookupKey k v of
  Just (String s) -> s
  Just (Number n) -> T.pack (show (fromMaybe (0::Int) (toBoundedInteger n)))
  _               -> ""

hasKey :: Text -> Value -> Bool
hasKey k (Object o) = KM.member (K.fromText k) o
hasKey _ _          = False

entryIds :: Value -> [Text]
entryIds (Object o) = [ K.toText k | k <- KM.keys o, K.toText k `notElem` reserved ]
entryIds _          = []

lookupEntry :: Text -> Value -> Value
lookupEntry eid (Object o) = fromMaybe (Object KM.empty) (KM.lookup (K.fromText eid) o)
lookupEntry _ _            = Object KM.empty

entryName :: Text -> Value -> Text
entryName eid def = case getStr "name" def of
  "" -> T.toTitle (T.replace "_" " " eid)
  n  -> n

catTitle :: Text -> Value -> Text
catTitle key def = case getStr "description" def of
  d | not (T.null d), T.length d <= 60, '\n' `notElem` T.unpack d -> T.strip d
  _ -> T.toUpper (T.replace "_" " " key)

valueToTexts :: Value -> [Text]
valueToTexts (Array a) = [ t | String t <- V.toList a ]
valueToTexts _         = []

-- ---------------------------------------------------------------------------
-- RNG helpers (in the Resolve monad)
-- ---------------------------------------------------------------------------

-- | Roll a dice expression.
roll :: Text -> Resolve Int
roll expr = state $ \(sh, g) -> let (n, g') = rollExpr expr g in (n, (sh, g'))

rollExpr :: Text -> StdGen -> (Int, StdGen)
rollExpr expr g =
  let s = T.strip (T.toLower expr)
  in  case readMaybe (T.unpack s) of
        Just n  -> (n, g)
        Nothing
          | "-" `T.isInfixOf` s, "d" `notElem` T.chunksOf 1 s -> rollRange s g
          | otherwise -> rollDice s g

rollRange :: Text -> StdGen -> (Int, StdGen)
rollRange s g = case T.splitOn "-" s of
  [lo, hi] | Just a <- readMaybe (T.unpack lo)
            , Just b <- readMaybe (T.unpack hi) -> uniformR (a, b) g
  _ -> (0, g)

rollDice :: Text -> StdGen -> (Int, StdGen)
rollDice s g0 =
  let (base, modStr) = T.breakOn "+" (T.replace "-" "+-" s)
      modifier = case T.stripPrefix "+" modStr of
                   Just r  -> fromMaybe 0 (readMaybe (T.unpack r))
                   Nothing -> 0
      (nStr, rest)   = T.breakOn "d" base
      sidesAndDrop   = T.drop 1 rest
      (sidesStr, dl) = T.breakOn "dl" sidesAndDrop
      n     = fromMaybe 1   (readMaybe (T.unpack nStr))
      sides = fromMaybe (6::Int) (readMaybe (T.unpack sidesStr))
      dropL = case T.stripPrefix "dl" dl of
                Just d  -> fromMaybe 0 (readMaybe (T.unpack (T.takeWhile (/= 'd') d)))
                Nothing -> 0
      (rolls, g1) = rollNDie n sides g0
  in  (sum (drop dropL (List.sort rolls)) + modifier, g1)

rollNDie :: Int -> Int -> StdGen -> ([Int], StdGen)
rollNDie n sides = runState $ replicateM n (state (uniformR (1, sides)))

-- | Sample n distinct elements uniformly at random.
sampleN :: Int -> [a] -> Resolve [a]
sampleN n xs = state $ \(sh, g) ->
  let (chosen, g') = go (min n (length xs)) xs g
  in  (chosen, (sh, g'))
  where
    go 0 _    g = ([], g)
    go k pool g =
      let (i, g')   = uniformR (0, length pool - 1) g
          x         = pool !! i
          pool'     = take i pool ++ drop (i+1) pool
          (rest, g'') = go (k-1) pool' g'
      in  (x : rest, g'')

shuffled :: [a] -> Resolve [a]
shuffled xs = sampleN (length xs) xs
