-- | Builds the vendored, statically-linked libgit2 ('cbits/build-libgit2.sh')
-- before GHC needs to link against it. 'gitlib-effect.cabal' declares
-- @include-dirs@/@extra-lib-dirs@ as paths relative to this package's
-- directory (so the configure-time foreign-library check and normal GHC
-- compilation both resolve them correctly), but Cabal's final package-db
-- registration step rejects relative library-dirs outright ("makes no
-- sense as there is nothing for it to be relative to") -- a real concern
-- since a registered @.conf@ file can be read from a different cwd later.
-- 'confHook' below calls through to the default configure first (so the
-- library's 'BuildInfo' is the fully flag-resolved one, not the
-- unflattened conditional tree an earlier, wrong attempt mutated to no
-- effect), then rewrites the resulting 'LocalBuildInfo's relative dirs to
-- absolute ones computed from the actual configure-time cwd.
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Types.PackageDescription (PackageDescription(..))
import Distribution.Types.Library (Library(..))
import Distribution.Types.BuildInfo (BuildInfo(..))
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>), isAbsolute)
import System.Process (callProcess)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { preBuild = \args flags -> do
      callProcess "cbits/build-libgit2.sh" []
      preBuild simpleUserHooks args flags
  , confHook = \(gpd, hbi) flags -> do
      lbi <- confHook simpleUserHooks (gpd, hbi) flags
      cwd <- getCurrentDirectory
      let absolutize p = if isAbsolute p then p else cwd </> p
          fixBI bi = bi
            { includeDirs  = map absolutize (includeDirs bi)
            , extraLibDirs = map absolutize (extraLibDirs bi)
            }
          fixLib lib = lib { libBuildInfo = fixBI (libBuildInfo lib) }
          pd  = localPkgDescr lbi
          pd' = pd { library = fmap fixLib (library pd) }
      return lbi { localPkgDescr = pd' }
  }
