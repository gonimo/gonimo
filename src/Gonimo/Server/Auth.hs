{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Gonimo.Server.Auth where

import           Control.Exception             (SomeException)
import           Control.Lens
import           Control.Monad.Freer           (Eff, Member)
import           Control.Monad.Freer.Exception (Exc (..))
import           Control.Monad.Freer.Reader    (Reader (..), ask)
import           Database.Persist              (Entity (..), Key)
import           Gonimo.Server.DbEntities
import           Gonimo.Server.Effects
import           Gonimo.Util
import           Servant.Server                (err403)

data AuthData = AuthData { _accountEntity   :: Entity Account
                         -- Usually just one ore two - so using list lookup
                         -- should be fine.
                         , _allowedFamilies :: [FamilyId]
                         , _clientEntity    :: Entity Client
                         }
$(makeLenses ''AuthData)

type AuthReader = Reader AuthData
type AuthReaderMember r = Member AuthReader r

type AuthServerConstraint r = (AuthReaderMember r, ServerConstraint r)
type AuthServerEffects = Eff '[AuthReader, Exc SomeException, Server]


askAccountId :: AuthReaderMember r => Eff r (Key Account)
askAccountId = entityKey . _accountEntity <$> ask

authView :: AuthReaderMember r => Getter AuthData a -> Eff r a
authView g = (^. g) <$> ask

isFamilyMember :: FamilyId -> AuthData -> Bool
isFamilyMember fid = (fid `elem`) . _allowedFamilies

authorizeAuthData :: Member (Exc SomeException) r => (AuthData -> Bool) -> AuthData -> Eff r ()
authorizeAuthData check = kickOut . check
  where
    kickOut False = throwServant err403 -- Forbidden
    kickOut True = return ()

authorize :: (Member (Exc SomeException) r, AuthReaderMember r) =>  (AuthData -> Bool) -> Eff r ()
authorize check = authorizeAuthData check =<< ask
