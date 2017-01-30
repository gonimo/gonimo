import Distribution.Simple
import Distribution.Simple.Setup (BuildFlags(..), ConfigFlags(..))
import Distribution.PackageDescription (PackageDescription (..), FlagName(..))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo (..))
import Distribution.Simple.Program.Run
import Distribution.Verbosity (normal)

finishBuild ::  Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
finishBuild _ _ _ localBuildInfo = do
  let flags = configConfigurationsFlags . configFlags $ localBuildInfo
  let unFlagName = \(FlagName s) -> s
  let flagStrings = unFlagName . fst <$> filter snd flags
  let prog = emptyProgramInvocation { progInvokePath = "./postBuild.sh"
                                    , progInvokeArgs = flagStrings
                                    -- , progInvokeArgs = [ "-a", "./static/*", "./dist/build/gonimo-front/gonimo-front.jsexe/" ]
                                    -- , progInvokeCwd = Just "./"
                                    }
  runProgramInvocation normal prog
  -- _ <- system "cp -a static/* dist/build/gonimo-front/gonimo-front.jsexe/"

cleanBuild ::  Args -> BuildFlags -> IO ()
cleanBuild _ _ = do
  let prog = emptyProgramInvocation { progInvokePath = "./preBuild.sh"
                                    }
  runProgramInvocation normal prog
  pure (Nothing, [])

main = do
  putStrLn "In Setup.hs"
  defaultMainWithHooks $ simpleUserHooks { postBuild = finishBuild
                                         , preBuild = cleanBuild
                                         }
