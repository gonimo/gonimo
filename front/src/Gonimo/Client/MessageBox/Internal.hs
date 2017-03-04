{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Gonimo.Client.MessageBox.Internal where

import Reflex.Dom.Core
import Control.Lens
import Data.Text (Text)
import qualified Gonimo.Client.App.Types as App
import qualified Gonimo.SocketAPI as API
import Gonimo.Db.Entities (FamilyId)
import Gonimo.Client.Server (webSocket_recv)

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

makeLenses ''Config
makeLenses ''MessageBox

fromApp :: Reflex t => App.Config t -> Config t
fromApp c = Config { _configMessage = (:[]) . ServerResponse <$> c^.App.server.webSocket_recv
                   }
