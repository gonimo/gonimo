{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}

import Reflex.Dom (run)

import qualified Gonimo.Client.Main as Gonimo

main :: IO ()
main = run Gonimo.main

