{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Git-backed interpreters for StoryStorage and StoryBranch.
--
-- Conventions owned by this layer (invisible to everything above):
--
--   * Branch refs live at  @refs/heads/story/<name>@
--   * Tick messages are encoded as:
--       @refs: <hash1> <hash2> ...\n<message>@   when refs are present
--       @<message>@                               otherwise
--   * Commits carry no tree changes — all commits use the empty git tree.
--   * @At@ is a stub: runs the action at head and returns an empty id mapping.
module Storyteller.Git
  ( runStoryStorageGit
  , runStoryBranchGit
  ) where

import Prelude hiding (drop)
import Data.Text (Text)
import qualified Data.Text as T
import Polysemy
import Polysemy.Fail

import Runix.Git

import Storyteller.Types
import Storyteller.Storage

-- ---------------------------------------------------------------------------
-- Naming conventions
-- ---------------------------------------------------------------------------

storyRef :: BranchName -> RefName
storyRef (BranchName n) = RefName ("refs/heads/story/" <> n)

-- | The well-known SHA of the empty git tree object.
emptyTree :: ObjectHash
emptyTree = ObjectHash "4b825dc642cb6eb9a060e54bf8d69288fbee4904"

-- ---------------------------------------------------------------------------
-- Message encoding / decoding
-- ---------------------------------------------------------------------------

-- | Encode tick refs and message into a commit message.
encodeMessage :: [TickId] -> Text -> Text
encodeMessage [] msg  = msg
encodeMessage refs msg =
  "refs: " <> T.unwords (map unTickId refs) <> "\n" <> msg

-- | Decode a commit message into (refs, message).
decodeMessage :: Text -> ([TickId], Text)
decodeMessage raw =
  case T.lines raw of
    (l:rest) | "refs: " `T.isPrefixOf` l ->
      let refs = map TickId $ T.words (T.drop 6 l)
      in (refs, T.intercalate "\n" rest)
    _ -> ([], raw)

-- ---------------------------------------------------------------------------
-- Conversion between git and tick vocabulary
-- ---------------------------------------------------------------------------

commitToTick :: ObjectHash -> CommitData -> Tick
commitToTick hash cd =
  let (refs, msg) = decodeMessage (commitMessage cd)
  in Tick
     { tickId      = TickId (unObjectHash hash)
     , tickParent  = TickId . unObjectHash <$> listToMaybe (commitParents cd)
     , tickRefs    = refs
     , tickMessage = msg
     }
  where
    listToMaybe []    = Nothing
    listToMaybe (x:_) = Just x

-- ---------------------------------------------------------------------------
-- StoryStorage interpreter
-- ---------------------------------------------------------------------------

-- | Interpret 'StoryStorage' against git via 'Runix.Git'.
runStoryStorageGit
  :: Members '[Git, Fail] r
  => Sem (StoryStorage : r) a
  -> Sem r a
runStoryStorageGit = interpret $ \case
  CreateBranch name -> do
    let ref = storyRef name
    -- Write a root commit (no parents) onto the empty tree
    rootHash <- writeCommit CommitData
      { commitParents = []
      , commitTree    = emptyTree
      , commitMessage = "root"
      }
    createRef ref rootHash
    return Branch { branchName = name, branchHead = TickId (unObjectHash rootHash) }

  DeleteBranch name ->
    deleteRef (storyRef name)

  ListBranches -> do
    pairs <- listRefs "refs/heads/story/"
    mapM resolveToHead pairs
    where
      resolveToHead (RefName ref, hash) =
        let name = BranchName $ T.drop (T.length "refs/heads/story/") ref
        in return Branch { branchName = name, branchHead = TickId (unObjectHash hash) }

  UpdateReferences _mapping ->
    -- Full cross-branch ref rewriting after rebase — deferred.
    return ()

-- ---------------------------------------------------------------------------
-- StoryBranch interpreter
-- ---------------------------------------------------------------------------

-- | Interpret 'StoryBranch' against git via 'Runix.Git', operating on the
--   named branch.
runStoryBranchGit
  :: Members '[Git, Fail] r
  => BranchName
  -> Sem (StoryBranch : r) a
  -> Sem r a
runStoryBranchGit branch = interpretH $ \case
      Store msg -> do
        headHash <- raise $ resolveHead branch
        newHash <- raise $ writeCommit CommitData
          { commitParents = [headHash]
          , commitTree    = emptyTree
          , commitMessage = encodeMessage [] msg
          }
        raise $ updateRef (storyRef branch) newHash
        pureT $ TickId (unObjectHash newHash)

      Drop -> do
        headHash <- raise $ resolveHead branch
        cd <- raise $ readCommit headHash
        case commitParents cd of
          []       -> pureT ()
          (p : _)  -> raise (updateRef (storyRef branch) p) >> pureT ()

      Get -> do
        headHash <- raise $ resolveHead branch
        cd <- raise $ readCommit headHash
        pureT $ commitToTick headHash cd

      Follow seed step -> do
        headHash <- raise $ resolveHead branch
        result <- raise $ walkFrom seed step headHash
        pureT result

      At tid action -> do
        -- 1. Collect the tail: commits from head back to (but not including) tid,
        --    in reverse order (oldest first after reversal).
        headHash <- raise $ resolveHead branch
        tail_ <- raise $ collectTail (TickId (unObjectHash headHash)) tid []
        -- 2. Rewind branch pointer to tid.
        raise $ updateRef (storyRef branch) (ObjectHash (unTickId tid))
        -- 3. Run the inner action with branch pointing at tid.
        fa <- runTSimple action
        -- 4. Replay the tail on top of whatever the action left at head.
        newHead <- raise $ resolveHead branch
        mapping <- raise $ replayTail newHead tail_
        -- 5. Update the branch to the replayed head.
        case mapping of
          [] -> return ()
          _  -> raise $ updateRef (storyRef branch) (ObjectHash (unTickId (snd (last mapping))))
        return $ fmap (, mapping) fa

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

resolveHead :: Members '[Git, Fail] r => BranchName -> Sem r ObjectHash
resolveHead name = do
  mhash <- resolveRef (storyRef name)
  case mhash of
    Just h  -> return h
    Nothing -> fail $ "branch not found: " <> T.unpack (unBranchName name)

-- | Walk from head back to (but not including) the target tick, accumulating
--   commits in reverse order (so the result is oldest-first).
collectTail
  :: Members '[Git, Fail] r
  => TickId       -- ^ current position (starts at head)
  -> TickId       -- ^ stop before this tick
  -> [CommitData] -- ^ accumulator
  -> Sem r [CommitData]
collectTail current stop acc
  | current == stop = return acc
  | otherwise = do
      cd <- readCommit (ObjectHash (unTickId current))
      case commitParents cd of
        [] -> fail "StoryBranch.At: target tick not found in branch history"
        (p:_) -> collectTail (TickId (unObjectHash p)) stop (cd : acc)

-- | Replay a list of commits (oldest-first) on top of the current head,
--   returning the old→new TickId mapping.
replayTail
  :: Members '[Git, Fail] r
  => ObjectHash        -- ^ current head to build on top of
  -> [CommitData]      -- ^ tail to replay, oldest first
  -> Sem r [(TickId, TickId)]
replayTail _ [] = return []
replayTail parent (cd : rest) = do
  let oldId = TickId $ unObjectHash $ case commitParents cd of { (p:_) -> p; [] -> parent }
  newHash <- writeCommit cd { commitParents = [parent] }
  let newId = TickId (unObjectHash newHash)
  remaining <- replayTail newHash rest
  return ((oldId, newId) : remaining)

walkFrom
  :: Members '[Git, Fail] r
  => b
  -> (b -> Tick -> (b, Maybe TickId))
  -> ObjectHash
  -> Sem r b
walkFrom acc step hash = do
  cd <- readCommit hash
  let tick = commitToTick hash cd
      (acc', next) = step acc tick
  case next of
    Nothing          -> return acc'
    Just (TickId h)  -> walkFrom acc' step (ObjectHash h)
