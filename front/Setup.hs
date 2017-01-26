import Distribution.Simple
import Distribution.Simple.Setup (BuildFlags(..))
import Distribution.PackageDescription (PackageDescription (..))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo (..))
import System.Process (system)

finishBuild ::  Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
finishBuild args buildFlags packageDescription localBuildInfo = do
  _ <- system "cp -a static/* dist/build/gonimo-front/gonimo-front.jsexe/"
  pure ()


main = do
  putStrLn "In Setup.hs"
  defaultMainWithHooks $ simpleUserHooks { postBuild = finishBuild }
