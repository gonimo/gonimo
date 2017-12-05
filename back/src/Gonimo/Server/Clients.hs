{-|
Module      : Gonimo.Server.Clients
Description : Handle and serve clients.
Copyright   : (c) Robert Klotzner, 2017
This module implements the http/webSocket server for actually serving the
clients and making them part of the server FRP network.
-}
module Gonimo.Server.Clients ( -- * Types
                             , Clients(..)
                             ) where



import Gonimo.Server.Clients.Internal


