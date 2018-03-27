{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -Wno-orphans          #-}
{-|
Module      : Gonimo.Server.Invitation
Description : Short description
Copyright   : (c) Robert Klotzner, 2018
Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module Gonimo.Server.Invitation ( -- * Types
                                  Invitation(..)
                                ) where


import qualified Codec.Binary.Base32         as Base32
import           Control.Concurrent.STM      (STM, retry)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar, readTVar)
import           Control.Lens
import           Data.ByteString             (ByteString)
import           Data.IndexedTable           (IndexedTable)
import qualified Data.IndexedTable           as Table
import           Data.Map.Strict             (Map)
import qualified Data.Set                    as Set
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Data.Time.Clock             (UTCTime, diffUTCTime)


import           Gonimo.Prelude
import           Gonimo.Server.Config
import           Gonimo.Server.CRUD          as CRUD
import qualified Gonimo.Server.Db.Account    as Account
import qualified Gonimo.Server.Db.Device     as Device
import qualified Gonimo.Server.Db.Family     as Family
import qualified Gonimo.Server.Db.Invitation as Db
import           Gonimo.Server.Error

import           Gonimo.SocketAPI.Invitation
import           Gonimo.SocketAPI.Types      as API
import           Gonimo.Server.Auth (HasAuthData, authorize, isFamilyMember, isAccount)

-- import Gonimo.Server.CodeInvitation.Type

-- | We use Base32 based codes, where each digit responds to 5 bits.
--
--   Currently we use 6 digits, resulting in about 10^9 possibilities.
codeLength :: Int
codeLength = 6

-- | Timeout in seconds until the code can be considered invalid.
codeValidTimeout :: Int
codeValidTimeout = 30

data LiveCode = LiveCode {
                           -- | Time stamp when the code was created.
                           _codeCreated :: UTCTime

                           -- | The actual 'InvitationCode'.
                         , _codeCode    :: InvitationCode
                         }

$(makeLenses 'LiveCode)

type LiveCodes = IndexedTable InvitationCode Map InvitationId LiveCode

data CodeInvitation = CodeInvitation
  {
    -- | Id to Code mapping.
    _liveCodes   :: TVar LiveCodes
  }

$(makeClassy 'CodeInvitation)


instance ( HasConfig env
         , HasCodeInvitation env
         , HasAuthData env
         ) => IsResourceHandler InvitationR (RIO env) where

  handleUpdate rInvId updData = do
    (invId, inv) <- readBy rInvId
    upd <- case updData of
      UpdateCode -> do
        authorize =<< isFamilyMember (inv ^. familyId)
        UpdatedCode <$> makeCode invId

      UpdateReceiver aid -> do
        authorize $ isSecretId rInvId
        authorize =<< isAccount aid
        UpdatedReceiver aid <$> updateReceiver invId aid

    pure (invId, upd)

  handleRead rInvId readData = do
    r@(invId, full) <- readBy rInvId

    isMember <- isFamilyMember (full ^. familyId)
    isAccount' <- fmap (fromMaybe False) . traverse isAccount $ full ^. receiverId
    authorize (isMember || isAccount')

    case readData of
      ReadFull -> pure $ DidReadFull <$> r
      ReadInfo  -> runDb $ do
        invFamily  <- Family.get $ _familyId full
        invDevice  <- Device.get $ _senderId full
        mInvUser   <- Account.getUser (deviceAccountId invDevice)
        let info = InvitationInfo
                   { _infoFamily = API.familyName invFamily
                   , _infoSendingDevice = fromMaybe "" $ deviceName invDevice
                   , _infoSendingUser = userLogin . snd <$> mInvUser
                   , _infoSecret = _secret full
                   }
        pure $ (invId, DidReadInfo info)

makeCode :: (HasConfig env, HasCodeInvitation env) => InvitationId -> RIO env InvitationCode
makeCode invId = do
    env <- ask
    now <- getCurrentTime
    atomically $ do
      removeCode env invId
      insertCode env invId now =<< getUntakenCode env

readBy :: (HasConfig env, HasCodeInvitation env) => ReadBy -> RIO env (InvitationId, Invitation)
readBy readId = do
  (invId, raw) <-
    case readId of
      ReadById invId -> (invId, ) <$> runDb (Db.get invId)
      ReadBySecret secret' -> runDb $ Db.getBySecret secret'
      ReadByCode code -> do
        invId <- getIdByCode code
        inv <- runDb $ Db.get invId
        pure (invId, inv)
  mCode <- getCodeById invId
  pure $ (invId, raw { _code = mCode })

-- | Is the query by some secret or with a plain db id.
isSecretId :: ReadBy -> Bool
isSecretId (ReadBySecret _) = True
isSecretId (ReadByCode _)   = True
isSecretId _                = False

-- | Update the receiver of an invitation.
--
--   This is only allowed if the receiver was not set already or equals the currently set receiver.
--   The invitation is claimed now and can no longer be retrieved by other parties.
updateReceiver :: (HasAuthData env, HasConfig env) => InvitationId -> AccountId -> RIO env InvitationInfo
updateReceiver invId aid = do
  runDb $ do
    inv <- Db.claim invId aid
    invFamily  <- Family.get $ _familyId inv
    invDevice  <- Device.get $ _senderId inv
    mInvUser   <- Account.getUser (deviceAccountId invDevice)
    pure $ InvitationInfo
                { _infoFamily = API.familyName invFamily
                , _infoSendingDevice = fromMaybe "" $ deviceName invDevice
                , _infoSendingUser = userLogin . snd <$> mInvUser
                , _infoSecret = _secret inv
                }


-- | Retrieve the code of an invitation.
--
--   Nothing if no valid code exists.
getCodeById :: HasCodeInvitation env => InvitationId -> RIO env (Maybe InvitationCode)
getCodeById invId = do
  currentCodes <- atomically . readTVar =<< view liveCodes
  now <- getCurrentTime
  pure $ currentCodes ^? at invId . _Just . to (getValidCode now) . _Just

-- | Retrieve code, Nothing if no longer valid.
getValidCode :: UTCTime -> LiveCode -> Maybe InvitationCode
getValidCode t liveCode =
  let
    code = liveCode ^. codeCode
    isValid = diffUTCTime t (liveCode ^. codeCreated) < fromIntegral codeValidTimeout
  in
    if isValid then Just code else Nothing

-- | Retrieve an 'InvitationId' by a given 'InvitationCode'.
--
--   Throws 'NoSuchInvitationCode' if no such code exists.
getIdByCode :: (HasCodeInvitation env) => InvitationCode -> RIO env InvitationId
getIdByCode invCode = do
  currentCodes <- atomically . readTVar =<< view liveCodes
  let mId = Table.getIndex currentCodes ^? at invCode . _Just . to Set.toList . to listToMaybe . _Just
  fromMaybeErr NoSuchInvitationCode mId

-- * STM helpers:

-- | Get a code that is currently not in use:
getUntakenCode ::  (HasConfig env, HasCodeInvitation env) => env -> STM InvitationCode
getUntakenCode env = do
  code <- generateInvitationCode env
  currentCodes <- readTVar $ env ^. liveCodes

  let codeAlreadyTaken = isJust $ Table.getIndex currentCodes ^. at code
  when codeAlreadyTaken retry
  pure code

insertCode :: (HasConfig env, HasCodeInvitation env)
  => env -> InvitationId -> UTCTime -> InvitationCode -> STM InvitationCode
insertCode env invId now code = do
  modifyTVar (env ^. liveCodes) (at invId .~ Just (LiveCode now code))
  pure code

removeCode :: (HasConfig env, HasCodeInvitation env) => env -> InvitationId ->  STM ()
removeCode env invId = do
  modifyTVar (env ^. liveCodes) $ at invId .~ Nothing



-- | Generate a random invitation code.
--
--   An Invitation code is just Text consisting of 6 characters. (Base32 characters)
generateInvitationCode :: HasConfig env => env -> STM InvitationCode
generateInvitationCode env = makeCode' <$> genRandomBytesSTM env codeByteCount
  where
    makeCode' :: ByteString -> InvitationCode
    makeCode' = InvitationCode . T.take codeLength . T.decodeUtf8 . Base32.encode

-- | How many random bytes do we need for our 'codeLength' ?
codeByteCount :: Int
codeByteCount = ceiling $ (fromIntegral codeLength * 5 / (8 :: Double))
