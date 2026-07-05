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
--
-- Deliberately writes to server stdout via 'embed', not through the
-- 'Runix.Logging.Logging' effect: every connection's command loop runs
-- under 'Server.Writer.Run.loggingWS', which forwards every 'Logging' call
-- straight to the *connected client* as an @agent.log@ push (that's the
-- right sink for user-facing agent progress messages, e.g. "writer agent
-- starting"). Routing per-command bookkeeping through the same channel
-- would spam that UI on every single command, including plain non-agent
-- ones like an atom edit — this is server operator visibility, a different
-- concern with a different, stdout, sink.
module Server.Core.Logging
  ( logCommand
  ) where

import Data.Time (diffUTCTime)
import qualified Data.Text as T
import Polysemy (Embed, Members, Sem, embed)
import Polysemy.Error (Error, catch, throw)
import Runix.Time (Time, getCurrentTime)

-- | Log @label@ as started, run @action@, then log it as finished (or
--   failed) together with how long it took. Failures are logged and
--   rethrown unchanged — the caller's own 'catch' (reporting the error to
--   the client) still runs exactly as it did before this wraps it.
logCommand :: Members '[Embed IO, Time, Error String] r => T.Text -> Sem r a -> Sem r a
logCommand label action = do
  embed $ putStrLn ("request: " <> T.unpack label)
  startedAt <- getCurrentTime
  result <- action `catch` \(err :: String) -> do
    finishedAt <- getCurrentTime
    embed $ putStrLn ("request failed: " <> T.unpack label <> " (" <> elapsed startedAt finishedAt <> "): " <> err)
    throw @String err
  finishedAt <- getCurrentTime
  embed $ putStrLn ("request finished: " <> T.unpack label <> " (" <> elapsed startedAt finishedAt <> ")")
  return result
  where
    elapsed a b = show (diffUTCTime b a)
