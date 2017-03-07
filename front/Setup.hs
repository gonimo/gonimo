{-# LANGUAGE CPP #-}
import Distribution.Simple
import Distribution.Simple.Setup (BuildFlags(..), ConfigFlags(..))
import Distribution.PackageDescription (PackageDescription (..), FlagName(..), HookedBuildInfo)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo (..))
import Distribution.Simple.Program.Run
import Distribution.Verbosity (normal)
import Distribution.Compiler (CompilerInfo(..), CompilerId(..), CompilerFlavor(..))

-- Can't use __GHCJS__ because Setup.hs might get build with ghc (nix does this!)
-- #ifdef GHCJS_COMPILER
finishBuild ::  Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
finishBuild _ _ _ localBuildInfo = do
  let flags = configConfigurationsFlags . configFlags $ localBuildInfo
  let (CompilerId flav _) = compilerInfoId . compilerInfo . compiler $ localBuildInfo
  putStrLn "Found flavour: "
  print flav
  let script = case flav of
                 GHCJS -> "./postBuild.sh"
                 _ -> "./postBuildGhc.sh"

  let unFlagName = \(FlagName s) -> s
  let flagStrings = unFlagName . fst <$> (filter ((=="dev") . unFlagName . fst) . filter snd) flags
  let prog = emptyProgramInvocation { progInvokePath = script
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
  -- Can't use this because of bug in cabal (we cannot conditionally add custom setup dependencies):
  -- cwd <- getCurrentDirectory
  -- let source  = cwd </> "static"
  -- let dest = cwd </> "dist"
  -- removeFile dest
  -- createSymbolicLink source dest


main = do
  defaultMainWithHooks $ simpleUserHooks { postBuild = finishBuild
                                         , preBuild = cleanBuild
                                         }
