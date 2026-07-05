{-# LANGUAGE OverloadedStrings #-}

-- | Throughput comparison: in-process hashing ('Runix.Git.Hash.hashObject')
-- vs shelling out to @git hash-object@ per call ('GitCliHash.gitHashObject'
-- -- the implementation it replaces). Puts real ops/s numbers on the
-- round-trip the switch removes.
--
-- Not a correctness check -- see @gitlib-effect-test@/@GitHashSpec@ for
-- that. Never fails the build; it only reports numbers.
--
--   cabal bench gitlib-effect-hash-bench
module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import GHC.Clock (getMonotonicTime)
import Text.Printf (printf)

import GitCliHash (gitHashObject)
import Runix.Git.Hash (ObjectKind(Blob), hashObject)

-- | Iteration counts per path. The subprocess path spawns a real process
-- each call, so it gets far fewer iterations than the pure path -- both
-- are chosen so the whole benchmark finishes in a few seconds.
pureIterations, cliIterations :: Int
pureIterations = 20000
cliIterations  = 50

sizes :: [(String, Int)]
sizes =
  [ ("tiny   (32 B  -- one short line)",   32)
  , ("small  (512 B -- one atom)",         512)
  , ("medium (8 KiB)",                     8 * 1024)
  ]

main :: IO ()
main = do
  putStrLn "Object hashing throughput: in-process vs. subprocess `git hash-object`"
  putStrLn (replicate 78 '=')
  forM_ sizes $ \(label, n) -> do
    -- Each iteration's content is distinguished by its index (a numeric
    -- suffix) rather than reusing one fixed 'ByteString' -- otherwise the
    -- pure path's result is the same value every time, and GHC is free to
    -- compute it once and share it, timing "re-force an already-evaluated
    -- thunk" instead of N real hashes.
    let content i = BS.replicate n 0x41 <> BS8.pack (show (i :: Int))
    pureRate <- rate pureIterations (\i -> evaluate (hashObject Blob (content i)))
    cliRate  <- rate cliIterations  (\i -> gitHashObject Blob (content i) >>= evaluate)
    printf "%-38s pure: %10.0f ops/s   subprocess: %7.1f ops/s   speedup: %6.0fx\n"
      label pureRate cliRate (pureRate / cliRate)
  where
    rate :: Int -> (Int -> IO a) -> IO Double
    rate n action = do
      t0 <- getMonotonicTime
      forM_ [1 .. n] (\i -> () <$ action i)
      t1 <- getMonotonicTime
      pure (fromIntegral n / (t1 - t0))
