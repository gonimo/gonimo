{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
module Gonimo.Client.DeviceList.UI where

import           Control.Lens
import           Control.Monad.IO.Class            (liftIO)
import qualified Data.List                         as List
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           Data.Maybe                        (isJust)
import           Data.Monoid
import           Data.Set                          (Set)
import qualified Data.Set                          as Set
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           Data.Time.Clock                   (UTCTime)
import           Data.Time.Format                  (defaultTimeLocale,
                                                    formatTime)
import           Data.Time.LocalTime               (TimeZone,
                                                    getCurrentTimeZone,
                                                    utcToLocalTime)
import           Gonimo.Db.Entities                (AccountId, DeviceId)
import qualified Gonimo.SocketAPI                  as API
import qualified Gonimo.SocketAPI.Types            as API
import           Gonimo.Types                      (DeviceType (..), _Baby)
import           Reflex.Dom.Core

import qualified Gonimo.Client.App.Types           as App
import           Gonimo.Client.ConfirmationButton  (confirmationEl)
import           Gonimo.Client.DeviceList.Internal
import           Gonimo.Client.EditStringButton    (editDeviceName)
import           Gonimo.Client.Reflex.Dom          (makeClickable, addBtnAttrs)
import           Gonimo.Client.WebRTC.Channel     (ReceivingState (..), Channel)
import qualified Gonimo.Client.WebRTC.Channel     as Channel
import Gonimo.Client.Util
import Data.Maybe


ui :: forall m t. (HasWebView m, MonadWidget t m)
              => App.Loaded t -> DeviceList t -> Dynamic t (Map DeviceId (Channel t))
              -> Dynamic t (Set DeviceId)  -> m (UI t)
ui loaded deviceList' channels connected = do
    tz <- liftIO $ getCurrentTimeZone
    evUI <- dyn $ renderAccounts tz loaded
            <$> deviceList'^.deviceInfos
            <*> pure (deviceList'^.onlineDevices) -- don't re-render full table
            <*> pure channels
            <*> pure connected
            <*> loaded^.App.authData
    uiSwitchPromptly evUI

renderAccounts :: forall m t. (HasWebView m, MonadWidget t m)
              => TimeZone
              -> App.Loaded t
              -> NestedDeviceInfos t
              -> Dynamic t (Map DeviceId DeviceType)
              -> Dynamic t (Map DeviceId (Channel t))
              -> Dynamic t (Set DeviceId)
              -> API.AuthData
              -> m (UI t)
renderAccounts tz loaded allInfos onlineStatus channels connected authData = do
    let
      isSelf (aId, _) = aId == API.accountId authData
      (self, others) = List.partition isSelf . Map.toList $ allInfos

    allUis <- traverse renderAccount $ self <> others
    pure $ uiLeftmost allUis
  where
    removeConfirmationText isUs devName
      = dynText $ if isUs
                  then pure "This is you!\nReally leave your current family?"
                  else pure "Do you really want to remove device '" <> devName <> pure "' from the family?"
    -- Currently pretty dumb, once we have non anonymous accounts render those differently:
    -- Visible group non devices belonging to a single account. Also family removal is per account.
    renderAccount :: (AccountId, Dynamic t (Map DeviceId (Dynamic t (API.DeviceInfo))))
                     -> m (UI t)
    renderAccount (_, infos) = do
      rs <- dyn $ renderDevices <$> infos
      uiSwitchPromptly rs

    renderDevices :: Map DeviceId (Dynamic t (API.DeviceInfo))
                  -> m (UI t)
    renderDevices infos = do
        let
          isSelf (devId,_) = devId == API.deviceId authData
          (self, others) = List.partition isSelf . Map.toList $ infos

        selfUIS <- traverse (renderDevice True) self
        othersUIS <- traverse (renderDevice False) others
        let allUIS = selfUIS <> othersUIS
        pure $ uiLeftmost allUIS

    renderDevice ::  Bool
                  -> (DeviceId , Dynamic t API.DeviceInfo)
                  -> m (UI t)
    renderDevice isSelf (devId, devInfo) = mdo
        let
          mDevType = Map.lookup devId <$> onlineStatus
          mChannel = Map.lookup devId <$> channels
          mConnStatus = fmap Channel.worstState <$> mChannel
          needsAlert  = (fromMaybe False . fmap Channel.needsAlert) <$> mChannel
          devName = API.deviceInfoName <$> devInfo
          isConnected = Set.member devId <$> connected

          devClass :: Dynamic t Text
          devClass = mconcat
                      [ pure "device "
                      , pure (if isSelf then "device-self " else "")
                      , fmap (\isSel -> if isSel then "selected " else "") isSelected
                      , fmap (\isConn -> if isConn then "connected " else "") isConnected
                      , fmap ((\isOnline -> if isOnline then "active " else "") . isJust) mDevType
                      , fmap ((\isBaby -> if isBaby then "isBaby " else "") . isJust . (^?_Just._Baby)) mDevType
                      , fmap (\cStatus -> if cStatus == Just StateUnreliable then "connectionUnreliable " else "") mConnStatus
                      , fmap (\needsAlert' -> if needsAlert' then "connectionBroken " else "") needsAlert
                      -- , fmap (\cStatus -> if cStatus == Just StateReceiving then "connectionReliable " else "") mConnStatus
                      -- , pure (if isSelf then "info " else "")
                      ]

        (isSelected, ui')  <-
          elDynClass "div" devClass $ do
            elClass "div" "status" $ el "div" blank
            selectedClick <- makeClickable
              . elAttr' "div" ("class" =: "name" <> "role" =: "button") $ do
              let devNameClass = if isSelf then "device-self dev-name" else "dev-name"
              elClass "span" devNameClass $ dynText (API.deviceInfoName <$> devInfo)
              elClass "span" "dev-loc" $ do
                text (if isSelf then "that's you" else "")
                dynText (renderBabyName <$> mDevType)
            (connectClick, streamClick, disconnectClick) <-
              elClass "div" "buttons" $ do
                conC <- makeClickable . elAttr' "div" (addFullScreenBtnAttrs "connect next-action") $ text "Connect"
                streamC <- makeClickable . elAttr' "div" (addFullScreenBtnAttrs "stream") $ text "Stream"
                let disconnectOnBroken = fmap (\needsAlert' -> if needsAlert' then "disconnect connectionBroken " else "disconnect ")  needsAlert
                discC <- makeClickable . elDynAttr' "div" (addBtnAttrs <$> disconnectOnBroken) $ text "Disconnect"
                pure (conC, streamC, discC)
            (nameChangeClick, removeClick) <-
              elClass "div" "info" $ do
                elClass "span" "last-seen" . dynText
                  $ pure "Last Seen: "
                  <> (renderLocalTimeString . API.deviceInfoLastAccessed <$> devInfo)
                nameChanged <- editDeviceName ( makeClickable
                                              . elAttr' "span" (addBtnAttrs "edit")
                                              $ text "RENAME"
                                            )
                              (API.deviceInfoName <$> devInfo)
                removeRequested <- confirmationEl ( makeClickable
                                                    . elAttr' "span" (addBtnAttrs "delete")
                                                    $ text "REMOVE"
                                                  )
                                   (removeConfirmationText isSelf devName)
                pure (nameChanged, removeRequested)

            let renameReq = uncurry API.ReqSetDeviceName <$> fmap (devId,) nameChangeClick

            let
              removeReq = API.ReqLeaveFamily
                          <$> current (API.deviceInfoAccountId <$> devInfo)
                          <*> current (loaded^.App.selectedFamily)
              removeReqEv = tag removeReq removeClick

            let ui'' = UI { _uiRequest = mconcat . map (fmap (:[])) $ [renameReq, removeReqEv]
                          , _uiConnect = const devId <$> connectClick
                          , _uiDisconnect = const devId <$> disconnectClick
                          , _uiShowStream = const devId <$> streamClick
                          }
            isSelected' <- toggle False selectedClick
            pure (isSelected', ui'')
        pure ui'
      where
        renderLocalTimeString :: UTCTime -> Text
        renderLocalTimeString t =
          let
            locTime = utcToLocalTime tz t
          in
            T.pack $ formatTime defaultTimeLocale "%F %R" locTime

        renderBabyName :: Maybe DeviceType -> Text
        renderBabyName devType = case devType of
                                  Just (Baby name) -> name
                                  _                -> ""
