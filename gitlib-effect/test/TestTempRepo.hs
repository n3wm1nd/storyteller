-- | A throwaway real git repository for integration specs -- shared by
-- 'GitIOSpec' and 'GitBatchSpec' so both exercise the actual @git@ binary
-- rather than a mock.
module TestTempRepo (withTempRepo) where

import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)

withTempRepo :: (FilePath -> IO a) -> IO a
withTempRepo action = withSystemTempDirectory "gitlib-effect-spec" $ \dir -> do
  callProcess "git" ["-C", dir, "init", "-q"]
  action dir
