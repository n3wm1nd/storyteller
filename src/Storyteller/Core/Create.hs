{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Introduce a new, empty file into the tree as its own tick — a plain
-- composition of storage primitives, same category as
-- 'Storyteller.Core.Append'. Whatever content follows (a chat.append, an
-- agent write) lands as ordinary 'Storyteller.Core.Atom.Atom' ticks
-- afterward; this only records the path's introduction.
module Storyteller.Core.Create
  ( createFile
  ) where

import qualified Data.ByteString as BS
import Polysemy
import Polysemy.Fail (Fail)
import Runix.FileSystem (FileSystemWrite, writeFile)

import Storyteller.Core.Created (Created(..))
import Storyteller.Core.Git (BranchTag)
import Storyteller.Core.Storage (StoryBranch, storeAs)
import Storyteller.Core.Types (TickId)

import Prelude hiding (writeFile)

-- | Write @path@ as an empty file and commit that as a 'Created' tick.
createFile
  :: forall branch r
  .  Members '[ StoryBranch branch
              , FileSystemWrite (BranchTag branch)
              , Fail ] r
  => FilePath -> Sem r TickId
createFile path = do
  writeFile @(BranchTag branch) path BS.empty
  storeAs @branch (Created path)
