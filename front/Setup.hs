import Distribution.Simple
import Distribution.Simple.Setup (BuildFlags(..))
import Distribution.PackageDescription (PackageDescription (..))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo (..))
import System.IO

finishBuild ::  Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
finishBuild args buildFlags packageDescription localBuildInfo = do
  putStrLn "After build - juhu!"


main = do
  putStrLn "In Setup.hs"
  defaultMainWithHooks $ simpleUserHooks { postBuild = finishBuild }
