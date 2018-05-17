{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -Wno-orphans          #-}
{-|
Module      : Gonimo.Server.CodeInvitation
Description : Code for handling code invitations.
Copyright   : (c) Robert Klotzner, 2018
Code invitations are invitations based on a rather short and therefore also
short lived code, which can be entered by a user to become a new family member.
-}
module Gonimo.Server.CodeInvitation ( codeLength
                                    , codeValidTimeout
                                    , makeLiveCodes
                                    , makeCode
                                    , getIdByCode
                                    , getCodeById
                                    ) where


import qualified Codec.Binary.Base32         as Base32
import           Control.Concurrent          (forkIO, threadDelay)
import           Control.Concurrent.STM      (STM, retry)
import qualified Control.Concurrent.STM      as STM
import           Control.Concurrent.STM.TVar as STM (TVar, modifyTVar',
                                                     newTVarIO, readTVar)
import           Control.Lens
import           Data.ByteString             (ByteString)
import qualified Data.IndexedTable           as Table
import qualified Data.Map.Strict             as Map
import qualified Data.Set                    as Set
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Data.Time.Clock             as T (UTCTime, diffUTCTime,
                                                   getCurrentTime)


import           Gonimo.Prelude
import           Gonimo.Server.Config        as C hiding (forkIO)
import           Gonimo.Server.Error
import           Gonimo.SocketAPI.Types      as API

-- import Gonimo.Server.CodeInvitation.Type

-- | We use Base32 based codes, where each digit responds to 5 bits.
--
--   Currently we use 6 digits, resulting in about 10^9 possibilities.
codeLength :: Int
codeLength = 6

-- | Timeout in seconds until the code can be considered invalid.
codeValidTimeout :: Int
codeValidTimeout = 30


-- | Creates an empty table and a thread pruning old codes.
--
--   The spawned thread never quits!
makeLiveCodes :: IO (TVar LiveCodes)
makeLiveCodes = do
    codes <- newTVarIO (Table.fromRawTable (Just . _codeCode) Map.empty)
    now <- T.getCurrentTime
    void . forkIO $ do -- Never quit
      threadDelay $ codeValidTimeout * 1000000
      STM.atomically $ modifyTVar' codes (filterValid now)
    pure codes
  where
    isValid now liveCode = diffUTCTime now (liveCode ^. codeCreated) < fromIntegral codeValidTimeout

    filterValid :: UTCTime -> LiveCodes -> LiveCodes
    filterValid now =
      Table.fromRawTable (Just . _codeCode)
      . Map.fromList
      . filter (isValid now . snd)
      . Map.toList
      . Table.getInner

-- | Make a code for a given `InvitationId`.
makeCode :: HasConfig env => InvitationId -> RIO env InvitationCode
makeCode invId = do
    env <- ask
    now <- C.getCurrentTime
    atomically $ do
      removeCode env invId
      insertCode env invId now =<< getUntakenCode env

-- | Retrieve an 'InvitationId' by a given 'InvitationCode'.
--
--   Throws 'NoSuchInvitationCode' if no such code exists.
getIdByCode :: HasConfig env => InvitationCode -> RIO env InvitationId
getIdByCode invCode = do
  currentCodes <- atomically . readTVar =<< view configLiveCodes
  let mId = Table.getIndex currentCodes ^? at invCode . _Just . to Set.toList . to listToMaybe . _Just
  fromMaybeErr NoSuchInvitationCode mId


-- | Retrieve the code of an invitation.
--
--   Nothing if no valid code exists.
getCodeById :: HasConfig env => InvitationId -> RIO env (Maybe InvitationCode)
getCodeById invId = do
  currentCodes <- atomically . readTVar =<< view configLiveCodes
  pure $ currentCodes ^? at invId . _Just . codeCode


-- * STM helpers:

-- | Get a code that is currently not in use:
getUntakenCode ::  HasConfig env => env -> STM InvitationCode
getUntakenCode env = do
  code <- generateInvitationCode env
  currentCodes <- readTVar $ env ^. configLiveCodes

  let codeAlreadyTaken = isJust $ Table.getIndex currentCodes ^. at code
  when codeAlreadyTaken retry
  pure code

insertCode :: HasConfig env
  => env -> InvitationId -> UTCTime -> InvitationCode -> STM InvitationCode
insertCode env invId now code = do
  modifyTVar' (env ^. configLiveCodes) (at invId .~ Just (LiveCode now code))
  pure code

removeCode :: HasConfig env => env -> InvitationId ->  STM ()
removeCode env invId = do
  modifyTVar' (env ^. configLiveCodes) $ at invId .~ Nothing



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
