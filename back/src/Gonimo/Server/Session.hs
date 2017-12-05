{-|
Module      : Gonimo.Server.Session
Description : A running client session, it handles client messages and forwards server events to the client.
Copyright   : (c) Robert Klotzner, 2017
This is the backend for the "Gonimo.Server.Sessions" reflex module, which does the low-level plumbing for lifting everything to events and behaviors in "Gonimo.Server.Sessions".
-}
module Gonimo.Server.Session ( -- * Types
                            , Client(..)
                            ) where



import Gonimo.Server.Session.Internal



make :: (HasConfig c, Server.HasConfig c) => c -> IO ()
make c = undefined


