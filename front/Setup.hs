import Distribution.Simple
import Distribution.Simple.Setup (BuildFlags(..))
import Distribution.PackageDescription (PackageDescription (..))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo (..))
import Distribution.Simple.Program.Run
import Distribution.Verbosity (normal)

finishBuild ::  Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
finishBuild args buildFlags packageDescription localBuildInfo = do
  let prog = emptyProgramInvocation { progInvokePath = "./postBuild.sh"
                                    -- , progInvokeArgs = [ "-a", "./static/*", "./dist/build/gonimo-front/gonimo-front.jsexe/" ]
                                    -- , progInvokeCwd = Just "./"
                                    }
  runProgramInvocation normal prog
  -- _ <- system "cp -a static/* dist/build/gonimo-front/gonimo-front.jsexe/"


main = do
  putStrLn "In Setup.hs"
  defaultMainWithHooks $ simpleUserHooks { postBuild = finishBuild }
