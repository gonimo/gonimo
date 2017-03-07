{-# LANGUAGE CPP #-}
import Distribution.Simple
import Distribution.Simple.Setup (BuildFlags(..), ConfigFlags(..))
import Distribution.PackageDescription (PackageDescription (..), FlagName(..), HookedBuildInfo)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo (..))
import Distribution.Simple.Program.Run
import Distribution.Verbosity (normal)
-- #ifndef __GHCJS__
-- import System.Directory
-- import System.Posix.Files
-- #endif

#ifdef __GHCJS__
finishBuild ::  Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
finishBuild _ _ _ localBuildInfo = do
  let flags = configConfigurationsFlags . configFlags $ localBuildInfo
  let unFlagName = \(FlagName s) -> s
  let flagStrings = unFlagName . fst <$> (filter ((=="dev") . unFlagName . fst) . filter snd) flags
  let prog = emptyProgramInvocation { progInvokePath = "./postBuild.sh"
                                    , progInvokeArgs = flagStrings
                                    -- , progInvokeArgs = [ "-a", "./static/*", "./dist/build/gonimo-front/gonimo-front.jsexe/" ]
                                    -- , progInvokeCwd = Just "./"
                                    }
  runProgramInvocation normal prog
  -- _ <- system "cp -a static/* dist/build/gonimo-front/gonimo-front.jsexe/"

cleanBuild ::  Args -> BuildFlags -> IO HookedBuildInfo
cleanBuild _ _ = do
  let prog = emptyProgramInvocation { progInvokePath = "./preBuild.sh"
                                    }
  runProgramInvocation normal prog
  pure (Nothing, [])
#else
finishBuild ::  Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
finishBuild _ _ _ localBuildInfo = do
  let flags = configConfigurationsFlags . configFlags $ localBuildInfo
  let unFlagName = \(FlagName s) -> s
  let flagStrings = unFlagName . fst <$> filter snd flags
  let prog = emptyProgramInvocation { progInvokePath = "./postBuildGhc.sh"
                                    , progInvokeArgs = flagStrings
                                    }
  runProgramInvocation normal prog
  -- Can't use this because of bug in cabal (we cannot conditionally add custom setup dependencies):
  -- cwd <- getCurrentDirectory
  -- let source  = cwd </> "static"
  -- let dest = cwd </> "dist"
  -- removeFile dest
  -- createSymbolicLink source dest

cleanBuild ::  Args -> BuildFlags -> IO HookedBuildInfo
cleanBuild _ _ = pure (Nothing, [])
#endif

main = do
  defaultMainWithHooks $ simpleUserHooks { postBuild = finishBuild
                                         , preBuild = cleanBuild
                                         }
