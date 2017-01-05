{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Gonimo.Server.State.Types where


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

data SessionMessage
  = SessionGotStolen
  | SessionCreateChannel !FromId !Secret
  | SessionSendMessage !FromId !Secret !Text

data Session
  = Session { _sessionSend :: !(SessionMessage -> IO ())
            , _sessionType :: !DeviceType
            }
$(makeLenses ''Session)

data Online
  = Online { _onlineSessions :: Map DeviceId Session
           , _onlineFamilies :: Map FamilyId (Set DeviceId)
           }

$(makeLenses ''Online)


type OnlineVar = TVar Online

type UpdateFamilyT m a = StateT Online m a
type MayUpdateFamily a = UpdateFamilyT (MaybeT Identity) a
type UpdateFamily a = UpdateFamilyT Identity a

emptyOnline :: Online
emptyOnline = Online {
    _onlineSessions = Map.empty
  }
