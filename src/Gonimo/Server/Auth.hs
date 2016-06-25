{-# LANGUAGE TemplateHaskell #-}
module Gonimo.Server.Auth where

import           Control.Exception             (SomeException)
import           Control.Lens
import           Control.Monad.Freer           (Eff, Member)
import           Control.Monad.Freer.Exception (Exc (..))
import           Control.Monad.Freer.Reader    (Reader (..), ask)
import           Database.Persist              (Entity (..), Key)
import           Data.Map.Strict as M
import           Gonimo.Server.DbEntities
import           Gonimo.Server.Effects
import           Gonimo.Types
import           Gonimo.Util
import           Servant.Server                (err403)

data AuthData = AuthData { _accountEntity   :: Entity Account
                         , _allowedFamilies :: [FamilyId]
                         -- Usually just one ore two - so using list lookup
                         -- should be fine.
                         , _client          :: ClientId
                         }
$(makeLenses ''AuthData)

newtype InMemorySecrets = SecretDB { fetch :: Map (ClientId, ClientId) Secret}

putSecret :: ClientId -> ClientId -> Secret -> InMemorySecrets -> InMemorySecrets
-- putSecret inserts secret possibly overwrites existing secret, secrets with
-- oneself cid1 == cid2 are silently ignored
putSecret cid1 cid2 secret | cid1 < cid2 = SecretDB . M.insert (cid1,cid2) secret . fetch
                           | cid1 > cid2 = SecretDB . M.insert (cid2,cid1) secret . fetch
                           | otherwise   = id

getSecret :: ClientId -> ClientId -> InMemorySecrets -> Maybe Secret
getSecret cid1 cid2 | cid1 < cid2 = M.lookup (cid1,cid2) . fetch
                    | cid1 > cid2 = M.lookup (cid2,cid1) . fetch
                    | otherwise   = const Nothing

deleteSecret :: ClientId -> ClientId -> InMemorySecrets -> InMemorySecrets
deleteSecret cid1 cid2 | cid1 < cid2 = SecretDB . M.delete (cid1,cid2) . fetch
                       | cid1 > cid2 = SecretDB . M.delete (cid2,cid1) . fetch
                       | otherwise   = id

type AuthReader = Reader AuthData
type AuthReaderMember r = Member AuthReader r

type AuthServerConstraint r = (AuthReaderMember r, ServerConstraint r)
type AuthServerEffects = Eff '[AuthReader, Exc SomeException, Server]


askAccountId :: AuthReaderMember r => Eff r (Key Account)
askAccountId = entityKey . _accountEntity <$> ask


isFamilyMember :: FamilyId -> AuthData -> Bool
isFamilyMember fid (AuthData _ fids _) = fid `elem` fids

authorizeAuthData :: Member (Exc SomeException) r => (AuthData -> Bool) -> AuthData -> Eff r ()
authorizeAuthData check = kickOut . check
  where
    kickOut False = throwServant err403 -- Forbidden
    kickOut True = return ()

authorize :: (Member (Exc SomeException) r, AuthReaderMember r) =>  (AuthData -> Bool) -> Eff r ()
authorize check = authorizeAuthData check =<< ask
