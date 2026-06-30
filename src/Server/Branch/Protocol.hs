{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Protocol for /branch/{name} connections.
--
-- Branch connections carry branch-level state only: the file tree (paths, no
-- content). File content lives in /branch/{name}/{path} connections.
-- The branch name is implicit from the connection URL.
module Server.Branch.Protocol
  ( BranchCommand(..)
  , BranchEvent(..)
  , BranchTick(..)
  , TrackFile(..)
  ) where

import Data.Aeson hiding (Error)
import Data.Aeson.Types (Parser, Pair)
import qualified Data.Text as T

data TrackFile = TrackFile
  { trackFrom :: FilePath
  , trackTo   :: FilePath
  } deriving (Show)

instance FromJSON TrackFile where
  parseJSON = withObject "TrackFile" $ \o ->
    TrackFile <$> o .: "from" <*> o .: "to"

instance ToJSON TrackFile where
  toJSON tf = object [ "from" .= trackFrom tf, "to" .= trackTo tf ]

data BranchCommand
  = Track      { bcId :: Maybe T.Text, bcSource :: T.Text, bcFiles :: [TrackFile] }
  | CharGen    { bcId :: Maybe T.Text, bcPath :: FilePath, bcScenario :: T.Text, bcSeed :: Maybe Int }
  | ReadTicks  { bcId :: Maybe T.Text }
  | AddNote    { bcId :: Maybe T.Text, bcRefTickId :: T.Text, bcNoteText :: T.Text }
  | MoveTick   { bcId :: Maybe T.Text, bcTickId :: T.Text, bcAfterTickId :: Maybe T.Text }
  | DeleteTick { bcId :: Maybe T.Text, bcTickId :: T.Text }
  | ChatPrompt { bcId :: Maybe T.Text, bcPath :: FilePath, bcPromptText :: T.Text }
  deriving (Show)

instance FromJSON BranchCommand where
  parseJSON = withObject "BranchCommand" $ \o -> do
    t <- o .: "type" :: Parser T.Text
    i <- o .:? "id"
    case t of
      "track"       -> Track      i <$> o .: "source" <*> o .: "files"
      "chargen"     -> CharGen    i <$> o .: "path" <*> o .: "scenario" <*> o .:? "seed"
      "read.ticks"  -> pure (ReadTicks i)
      "add.note"    -> AddNote    i <$> o .: "refTickId" <*> o .: "text"
      "move.tick"   -> MoveTick   i <$> o .: "tickId" <*> o .:? "afterTickId"
      "delete.tick" -> DeleteTick i <$> o .: "tickId"
      "chat.prompt" -> ChatPrompt i <$> o .: "path" <*> o .: "text"
      _             -> fail ("unknown branch command: " <> T.unpack t)

-- | A tick as seen by the client — a sum type so the frontend can pattern-match
--   on kind and access only the fields that make sense for each variant.
--
--   'BranchTickAtom': a content tick; refs are cross-branch entity references.
--   'BranchTickNote': an annotation tick; ref is the single annotated atom tick id.
data BranchTick
  = BranchTickAtom
      { btTickId  :: T.Text
      , btParent  :: Maybe T.Text
      , btRefs    :: [T.Text]
      , btMessage :: T.Text
      , btAtomFile :: Maybe T.Text  -- ^ file path hint, present when the atom has a "file" field
      }
  | BranchTickNote
      { btTickId  :: T.Text
      , btParent  :: Maybe T.Text
      , btRef     :: T.Text   -- ^ the annotated atom tick id (first of tickRefs)
      , btText    :: T.Text   -- ^ the annotation text (message with "note: " stripped)
      }
  | BranchTickPrompt
      { btTickId  :: T.Text
      , btParent  :: Maybe T.Text
      , btFile    :: T.Text
      , btText    :: T.Text
      }
  deriving (Show)

instance ToJSON BranchTick where
  toJSON (BranchTickAtom tid par refs msg mFile) = object $
    [ "kind"    .= ("atom" :: T.Text)
    , "tickId"  .= tid
    , "parent"  .= par
    , "refs"    .= refs
    , "message" .= msg
    ] <> maybe [] (\f -> [("file", toJSON f)]) mFile
  toJSON (BranchTickNote tid par ref txt) = object
    [ "kind"   .= ("note" :: T.Text)
    , "tickId" .= tid
    , "parent" .= par
    , "ref"    .= ref
    , "text"   .= txt
    ]
  toJSON (BranchTickPrompt tid par file txt) = object
    [ "kind"   .= ("prompt" :: T.Text)
    , "tickId" .= tid
    , "parent" .= par
    , "file"   .= file
    , "text"   .= txt
    ]

data BranchEvent
  = BranchReady        { beId :: Maybe T.Text, beBranch :: T.Text, beFiles :: [FilePath] }
  | BranchTicks        { beTicks :: [BranchTick] }
  | FileAdded          { beId :: Maybe T.Text, bePath :: FilePath }
  | FileRemoved        { beId :: Maybe T.Text, bePath :: FilePath }
  | TicksInvalidated   { beId :: Maybe T.Text, beMapping :: [(T.Text, T.Text)] }
  | BranchError T.Text
  deriving (Show)

instance ToJSON BranchEvent where
  toJSON = \case
    BranchReady mid branch files ->
      object $ withId mid [ "type" .= ("branch.ready" :: T.Text)
                           , "branch" .= branch, "files" .= files ]
    BranchTicks ticks ->
      object [ "type" .= ("branch.ticks" :: T.Text), "ticks" .= ticks ]
    FileAdded mid path ->
      object $ withId mid [ "type" .= ("file.added"   :: T.Text), "path" .= path ]
    FileRemoved mid path ->
      object $ withId mid [ "type" .= ("file.removed" :: T.Text), "path" .= path ]
    TicksInvalidated mid mapping ->
      object $ withId mid
        [ "type"    .= ("ticks.invalidated" :: T.Text)
        , "mapping" .= map (\(a,b) -> object ["old" .= a, "new" .= b]) mapping ]
    BranchError msg ->
      object [ "type" .= ("error" :: T.Text), "message" .= msg ]

withId :: Maybe T.Text -> [Pair] -> [Pair]
withId Nothing  ps = ps
withId (Just i) ps = ("id" .= i) : ps
