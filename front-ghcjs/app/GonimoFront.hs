{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}

import qualified Gonimo.Client.Main as Gonimo

-- import qualified GHCJS.DOM.Types as JS


main :: IO ()
main = Gonimo.main =<< Gonimo.mkEmptyConfig

