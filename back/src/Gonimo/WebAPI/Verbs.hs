{-# LANGUAGE DeriveDataTypeable #-}
module Gonimo.WebAPI.Verbs where

import           Data.Typeable
import           Servant.API.Verbs

data RECEIVE = RECEIVE
    deriving (Read, Show, Eq, Typeable)

type Receive = Verb 'RECEIVE 200

instance ReflectMethod 'RECEIVE where
    reflectMethod _ = "RECEIVE"

