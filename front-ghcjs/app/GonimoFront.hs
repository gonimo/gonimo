{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Gonimo.Client.Host.Impl as Gonimo
import qualified Gonimo.Client.Main      as Gonimo

-- import qualified GHCJS.DOM.Types as JS


main :: IO ()
main = Gonimo.main =<< Gonimo.makeEmptyHostVars
