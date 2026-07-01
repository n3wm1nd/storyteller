{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.NotificationSpec (spec) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM (atomically, newTChanIO, writeTChan)
import Control.Monad (void)
import Polysemy (embed, runM)
import Test.Hspec

import Server.Notification (BranchNotification(..), watchBranch)

-- | Poll 'mv' until it holds at least 'n' elements or we give up. A fixed
--   'threadDelay' would either be too short under load or waste time when
--   the channel drains instantly; polling adapts to either.
waitFor :: MVar [a] -> Int -> IO [a]
waitFor mv n = go (200 :: Int)
  where
    go 0 = readMVar mv
    go k = do
      xs <- readMVar mv
      if length xs >= n then return xs else threadDelay 1000 >> go (k - 1)

spec :: Spec
spec = describe "watchBranch" $ do

  it "ignores notifications for other branches" $ do
    chan <- newTChanIO
    seen <- newMVar []
    tid  <- forkIO $ runM $ void $ watchBranch chan "mine" () $ \() ->
      embed (modifyMVar_ seen (return . (() :))) >> return ()
    atomically $ writeTChan chan (BranchNotification "other")
    atomically $ writeTChan chan (BranchNotification "also-not-mine")
    atomically $ writeTChan chan (BranchNotification "mine")
    got <- waitFor seen 1
    killThread tid
    length got `shouldBe` 1

  it "threads the accumulator through consecutive matching notifications, unaffected by non-matches" $ do
    chan <- newTChanIO
    seen <- newMVar []
    tid  <- forkIO $ runM $ void $ watchBranch chan "mine" (0 :: Int) $ \n -> do
      let n' = n + 1
      embed $ modifyMVar_ seen (return . (++ [n']))
      return n'
    atomically $ writeTChan chan (BranchNotification "mine")
    atomically $ writeTChan chan (BranchNotification "other")
    atomically $ writeTChan chan (BranchNotification "mine")
    atomically $ writeTChan chan (BranchNotification "mine")
    got <- waitFor seen 3
    killThread tid
    got `shouldBe` [1, 2, 3]
