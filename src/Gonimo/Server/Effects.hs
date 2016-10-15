{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{--
  Gonimo server uses the new effects API from the freer package. This
  is all IO effects of gonimo server will be modeled in an interpreter,
  which can then be interpreted in various ways, e.g. a interpreter for
  testing, one for development and one for production.
--}
module Gonimo.Server.Effects (
    Server
  , ServerConstraint
  , atomically
  , genRandomBytes
  , generateSecret
  , getCurrentTime
  , getState
  , logDebug
  , logError
  , logInfo
  , logMessage
  , logTH
  , logWarn
  , notify
  , updateFamilyRetryEff
  , updateFamilyErrEff
  , updateFamilyEff
  , getFamilyEff
  , runDb
  , runRandom
  , sendEmail
  , timeout
  ) where



import           Control.Concurrent.STM         (STM, retry)
import           Control.Exception              (SomeException)
import           Control.Monad.Freer            (Eff)
import           Control.Monad.Freer.Exception  (Exc)
import           Control.Monad.Logger           (LogLevel (..), LogSource,
                                                 ToLogStr, liftLoc)
import           Data.ByteString                (ByteString)
import           Data.Proxy
import           Data.Text
import           Data.Time.Clock                (UTCTime)
import           Database.Persist.Sql           (SqlBackend)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Network.Mail.Mime              (Mail)
import           Servant.Subscriber             (Event, HasLink, IsElem,
                                                 IsSubscribable,
                                                 IsValidEndpoint, MkLink, URI)
import           System.Random                  (StdGen)

import           Gonimo.Database.Effects
import           Gonimo.Server.DbEntities       (FamilyId)
import           Gonimo.Server.Effects.Internal
import           Gonimo.Server.Error            (ServerError (NoSuchFamily),
                                                 fromMaybeErr, throwServer)
import           Gonimo.Server.State            (FamilyOnlineState, OnlineState,
                                                 lookupFamily, updateFamily, UpdateFamily)
import           Gonimo.Server.Types            (Secret (..))
import           Gonimo.WebAPI                  (GonimoAPI)
import           Utils.Constants                (standardDelay)


secretLength :: Int
secretLength = 16


atomically :: ServerConstraint r => STM a -> Eff r a
atomically = sendServer . Atomically

timeout :: ServerConstraint r => Int -> ServerEffects a -> Eff r a
timeout n eff = sendServer $ Timeout n eff 

sendEmail :: ServerConstraint r => Mail -> Eff r ()
sendEmail = sendServer . SendEmail


logMessage :: (ServerConstraint r, ToLogStr msg)  => Loc -> LogSource -> LogLevel -> msg -> Eff r ()
logMessage loc ls ll msg = sendServer $ LogMessage loc ls ll msg


genRandomBytes :: ServerConstraint r => Int -> Eff r ByteString
genRandomBytes = sendServer . GenRandomBytes

getCurrentTime :: ServerConstraint r => Eff r UTCTime
getCurrentTime = sendServer GetCurrentTime


runDb :: ServerConstraint r => Eff '[Exc SomeException, Database SqlBackend]  a -> Eff r a
runDb = sendServer . RunDb

runRandom :: ServerConstraint r => (StdGen -> (a,StdGen)) -> Eff r a
runRandom = sendServer . RunRandom

generateSecret :: ServerConstraint r => Eff r Secret
generateSecret = Secret <$> genRandomBytes secretLength


getState :: ServerConstraint r => Eff r OnlineState
getState = sendServer GetState

notify :: forall endpoint r. (ServerConstraint r, IsElem endpoint GonimoAPI, HasLink endpoint
                      , IsValidEndpoint endpoint, IsSubscribable endpoint GonimoAPI)
                      => Event -> Proxy endpoint -> (MkLink endpoint -> URI) -> Eff r ()
notify ev pE cb = sendServer $ Notify ev pE cb

-- | Update family, retrying if updateF returns Nothing
updateFamilyRetryEff :: ServerConstraint r
                   => FamilyId -> UpdateFamily a -> Eff r a
updateFamilyRetryEff familyId updateF = do
  state <- getState
  timeout standardDelay . atomically $ do
    r <- updateFamily state familyId updateF
    case r of
      Nothing -> retry
      Just v -> return v

-- | Update family, throwing err if updateF returns Nothing
updateFamilyErrEff :: ServerConstraint r
                   => ServerError -> FamilyId -> UpdateFamily a -> Eff r a
updateFamilyErrEff err familyId updateF = do
  state <- getState
  r <- atomically $ updateFamily state familyId updateF
  case r of
    Nothing -> throwServer err
    Just v -> return v

-- | Update family, ignoring Nothing.
updateFamilyEff :: ServerConstraint r
                   => FamilyId -> UpdateFamily () -> Eff r ()
updateFamilyEff familyId updateF = do
  state <- getState
  atomically $ updateFamily state familyId updateF
  pure ()


getFamilyEff :: ServerConstraint r
                =>  FamilyId -> Eff r FamilyOnlineState
getFamilyEff familyId = do
  state <- getState
  mFamily <- atomically $ lookupFamily state familyId
  fromMaybeErr (NoSuchFamily familyId) mFamily

-- We can not create an instance of MonadLogger for (Member Server r => Eff r).
-- The right solution would be to define a Logger type together with an
-- interpreter, which is in fact a more flexible approach than typeclasses for
-- monads, but we don't live in this perfect world - so let's go the way of
-- least resistance and just redfine the TH functions we want to use:
logTH :: LogLevel -> Q Exp
logTH level =
    [|logMessage $(qLocation >>= liftLoc) (pack "") $(lift level)
     . (id :: Text -> Text)|]

logDebug :: Q Exp
logDebug = logTH LevelDebug

-- | See 'logDebug'
logInfo :: Q Exp
logInfo = logTH LevelInfo
-- | See 'logDebug'
logWarn :: Q Exp
logWarn = logTH LevelWarn
-- | See 'logDebug'
logError :: Q Exp
logError = logTH LevelError
