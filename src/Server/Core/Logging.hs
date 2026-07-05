{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Generic "what ran and how long it took" logging for the command-loop
-- handlers (File\/Branch\/Session connections). A request-agnostic wrapper
-- around whichever action dispatches one already-decoded command, rather
-- than each command's own handler having to remember to log its own
-- start\/finish — see each protocol's @commandKind@ for the label this is
-- meant to be called with.
module Server.Core.Logging
  ( logCommand
  ) where

import Data.Time (diffUTCTime)
import qualified Data.Text as T
import Polysemy (Members, Sem)
import Polysemy.Error (Error, catch, throw)
import Runix.Logging (Logging, info)
import Runix.Time (Time, getCurrentTime)

-- | Log @label@ as started, run @action@, then log it as finished (or
--   failed) together with how long it took. Failures are logged and
--   rethrown unchanged — the caller's own 'catch' (reporting the error to
--   the client) still runs exactly as it did before this wraps it.
logCommand :: Members '[Logging, Time, Error String] r => T.Text -> Sem r a -> Sem r a
logCommand label action = do
  info $ "command started: " <> label
  startedAt <- getCurrentTime
  result <- action `catch` \(err :: String) -> do
    finishedAt <- getCurrentTime
    info $ "command failed: " <> label <> " (" <> elapsed startedAt finishedAt <> "): " <> T.pack err
    throw @String err
  finishedAt <- getCurrentTime
  info $ "command finished: " <> label <> " (" <> elapsed startedAt finishedAt <> ")"
  return result
  where
    elapsed a b = T.pack (show (diffUTCTime b a))
