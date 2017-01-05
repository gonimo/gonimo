{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Gonimo.Server.Messenger.Internal where


import           Control.Concurrent.STM    (TVar)
import           Control.Lens
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State (StateT (..))
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map

import           Gonimo.Db.Entities (DeviceId, FamilyId)
import           Gonimo.Types (DeviceType)

type FromId = DeviceId
type ToId   = DeviceId

data Message
  = MessageGotStolen
  | MessageCreateChannel !FromId !Secret
  | MessageSendMessage !FromId !Secret !Text

data Receiver
  = Receiver { _receiverSend :: !(Message -> IO ())
             , _receiverType :: !DeviceType
             , _receiverFamily :: !(Maybe FamilyId)
             }
$(makeLenses ''Receiver)

data Messenger
  = Messenger { _messengerReceivers :: Map DeviceId Messenger
              , _messengerFamilies :: Map FamilyId (Set DeviceId)
              }

$(makeLenses ''Messenger)


type MessengerVar = TVar Messenger

emptyMessenger :: Messenger
emptyMessenger = Messenger {
    _messengerMessengers = Map.empty
  }
