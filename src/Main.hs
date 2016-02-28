module Main where

import Data.Text
import Gonimo.Server
import Gonimo.WebAPI
import Network.Wai
import Network.Wai.Handler.Warp
import Servant



app :: Application
app = serve gonimoAPI server

main :: IO ()
main = run 8081 app
