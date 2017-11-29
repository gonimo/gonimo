{-|
Module      : Gonimo.SocketAPI
Description : SocketAPI current version.
Copyright   : (c) Robert Klotzner, 2017
API used for client - server communication.
-}

module Gonimo.SocketAPI ( module Gonimo.SocketAPI.V1
                        , apiVersion
                        ) where


import Gonimo.SocketAPI.V1

apiVersion :: Int
apiVersion = 1
