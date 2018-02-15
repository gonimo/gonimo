{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Gonimo.Client.MessageBox.Internal where

import           Control.Lens
import           Data.Text               (Text)
import           Reflex.Dom.Core

import qualified Gonimo.Client.App.Types as App
import           Gonimo.Client.Server    hiding (Config)
import qualified Gonimo.SocketAPI        as API
import           Gonimo.SocketAPI.Types  (FamilyId)

invitationQueryParam :: Text
invitationQueryParam = "messageBox"

data Config t
  = Config { _configMessage :: Event t [ Message ]
           }

data MessageBox t
  = MessageBox { _action :: Event t [Action]
               }

data Message
  = ServerResponse API.ServerResponse

data Action
  = ServerRequest API.ServerRequest
  | SelectFamily !FamilyId

fromApp :: Reflex t => App.Config t -> Config t
fromApp c = Config { _configMessage = (:[]) . ServerResponse <$> c^.onResponse
                   }

-- Prisms:

_ServerRequest :: Prism' Action API.ServerRequest
_ServerRequest = prism' (\r -> ServerRequest r) go
  where
    go c = case c of
      ServerRequest r -> Just r
      _               -> Nothing

_SelectFamily :: Prism' Action FamilyId
_SelectFamily = prism' (\r -> SelectFamily r) go
  where
    go c = case c of
      SelectFamily r -> Just r
      _              -> Nothing

-- Lenses for Config t:

configMessage :: Lens' (Config t) (Event t [ Message ])
configMessage f config' = (\configMessage' -> config' { _configMessage = configMessage' }) <$> f (_configMessage config')


-- Lenses for MessageBox t:

action :: Lens' (MessageBox t) (Event t [Action])
action f messageBox' = (\action' -> messageBox' { _action = action' }) <$> f (_action messageBox')


