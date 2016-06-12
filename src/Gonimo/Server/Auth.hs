module Gonimo.Server.Auth where

import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Exception (Exc (..))
import Control.Monad.Freer.Reader (Reader (..), ask)
import Gonimo.Server.DbEntities
import Gonimo.Server.Effects
import Gonimo.Util
import Servant.Server (err403)
import Database.Persist (Entity(..), Key)
import Control.Exception (SomeException)

data AuthData = AuthData { authDataAccountEntity :: Entity Account
                         , authDataAllowedFamilies :: [FamilyId]
                         -- Usually just one ore two - so using list lookup
                         -- should be fine.
                         , authDataClient :: ClientId
                         }

type AuthReader = Reader AuthData
type AuthReaderMember r = Member AuthReader r

type AuthServerConstraint r = (AuthReaderMember r, ServerConstraint r)
type AuthServerEffects = Eff '[AuthReader, Exc SomeException, Server]


askAccountId :: AuthReaderMember r => Eff r (Key Account)
askAccountId = entityKey . authDataAccountEntity <$> ask


isFamilyMember :: FamilyId -> AuthData -> Bool
isFamilyMember fid (AuthData _ fids _) = fid `elem` fids

authorizeAuthData :: Member (Exc SomeException) r => (AuthData -> Bool) -> AuthData -> Eff r ()
authorizeAuthData check = kickOut . check
  where
    kickOut False = throwServant err403 -- Forbidden
    kickOut True = return ()

authorize :: (Member (Exc SomeException) r, AuthReaderMember r) =>  (AuthData -> Bool) -> Eff r ()
authorize check = authorizeAuthData check =<< ask
