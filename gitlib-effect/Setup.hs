-- | Builds the vendored, statically-linked libgit2 ('cbits/build-libgit2.sh')
-- before GHC needs to link against it.
--
-- The build happens inside 'confHook', ahead of the default configure, not
-- in 'preBuild': Cabal's configure step itself probes for the @git2@
-- foreign library named in @extra-libraries@, and fails outright ("Missing
-- (or bad) C library: git2") if it isn't there yet -- so by 'preBuild' it is
-- already too late. The distinction only shows up on a genuinely clean
-- build, which is why it went unnoticed: once @cbits/build/install@ exists,
-- the script short-circuits and every later configure finds the library
-- regardless of which hook would have produced it.
--
-- 'gitlib-effect.cabal' declares
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
--
-- Those absolute paths point into this package's build tree, though, which
-- is only good for as long as the build tree lives -- not true of an
-- installed package in general, and never true of a store build, where the
-- tree is a scratch directory cabal deletes. So 'postCopy' below also
-- installs the archive into the library component's own libdir, a directory
-- registration always records and which outlives the build.
import Distribution.Simple
import Distribution.Simple.Flag (fromFlagOrDefault)
import Distribution.Simple.InstallDirs (CopyDest(NoCopyDest), InstallDirs(libdir))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..), absoluteComponentInstallDirs, withLibLBI)
import Distribution.Simple.Setup (CopyFlags(copyDest))
import Distribution.Types.ComponentLocalBuildInfo (componentUnitId)
import Distribution.Types.PackageDescription (PackageDescription(..))
import Distribution.Types.Library (Library(..))
import Distribution.Types.BuildInfo (BuildInfo(..))
import System.Directory (copyFile, createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath ((</>), isAbsolute)
import System.Process (callProcess)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { confHook = \(gpd, hbi) flags -> do
      -- Run through bash rather than exec'ing the script: cabal's sdist
      -- stores every file 0644, so from a tarball -- the shape a
      -- @cabal install@ builds -- it is not executable.
      callProcess "bash" ["cbits/build-libgit2.sh"]
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

    -- Install libgit2.a into the library component's own libdir, alongside
    -- the Haskell archive. Registration always lists that directory in the
    -- package's library-dirs, so @-lgit2@ keeps resolving for consumers once
    -- this build tree is gone -- which is the normal case for an installed
    -- package, and unavoidable for a store build, where the tree was a
    -- scratch directory. 'withLibLBI' rather than a direct component lookup
    -- because this hook also fires for per-component builds of the test
    -- suite and benchmarks, whose 'LocalBuildInfo' has no library in it.
  , postCopy = \args flags pd lbi -> do
      withLibLBI pd lbi $ \_ libClbi -> do
        let dest   = fromFlagOrDefault NoCopyDest (copyDest flags)
            dstDir = libdir (absoluteComponentInstallDirs pd lbi
                               (componentUnitId libClbi) dest)
        createDirectoryIfMissing True dstDir
        copyFile ("cbits" </> "build" </> "install" </> "lib" </> "libgit2.a")
                 (dstDir </> "libgit2.a")
      postCopy simpleUserHooks args flags pd lbi
  }
