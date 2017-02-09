{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
module Gonimo.Client.DeviceList.UI where

import           Control.Lens
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Class         (lift)
import           Control.Monad.Trans.Maybe         (MaybeT (..), runMaybeT)
import qualified Data.List                         as List
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           Data.Maybe                        (fromMaybe)
import           Data.Monoid
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
import           Gonimo.Types                      (DeviceType (..))
import           Reflex.Dom
import           Safe                              (headMay)

import qualified Gonimo.Client.App.Types           as App
import           Gonimo.Client.ConfirmationButton  (confirmationButton)
import           Gonimo.Client.DeviceList.Internal
import           Gonimo.Client.EditStringButton    (editStringButton)

-- TODO: At removal of one self - add note: "this is you"
-- Overrides configCreateDeviceList && configLeaveDeviceList
ui :: forall m t. (HasWebView m, MonadWidget t m)
            => App.Loaded t -> DeviceList t -> m (Event t [API.ServerRequest])
ui loaded deviceList' = do
    (renameEv, removeEv) <- renderList loaded deviceList'
    let renameReq = uncurry API.ReqSetDeviceName <$> renameEv
    let removeReq = uncurry (flip API.ReqLeaveFamily)
                    <$> attach (current (loaded^.App.selectedFamily)) removeEv
    pure $  mconcat (fmap (:[]) <$> [renameReq, removeReq])

renderList :: forall m t. (HasWebView m, MonadWidget t m)
              => App.Loaded t -> DeviceList t
              -> m (Event t (DeviceId, Text), Event t AccountId)
renderList loaded deviceList' = do
  elClass "table" "table table-striped" $ do
    el "thead" $ do
      elClass "th"  "centered" $ text "Online"
      -- elClass "th"  "centered" $ text "Type"
      el "th" $ text "Name"
      el "th" $ text "Last Seen"
      el "th" blank
      el "th" blank
      el "th" blank
    tz <- liftIO $ getCurrentTimeZone
    evEvents <- el "tbody" $ do
      evEv <- dyn $ renderAccounts tz
                <$> deviceList'^.deviceInfos
                <*> pure (deviceList'^.onlineDevices) -- don't re-render full table
                <*> loaded^.App.authData
      el "tr" $ do
        elAttr "td" ("colspan" =: "5" <> "style" =: "text-align: right;") $ text "+"
      pure evEv
        

    (,) <$> switchPromptly never (fst <$> evEvents)
        <*> switchPromptly never (snd <$> evEvents)

renderAccounts :: forall m t. (HasWebView m, MonadWidget t m)
              => TimeZone
              -> NestedDeviceInfos t
              -> Dynamic t (Map DeviceId DeviceType)
              -> API.AuthData
              -> m (Event t (DeviceId, Text), Event t AccountId)
renderAccounts tz allInfos onlineStatus authData = do
    let
      isSelf (aId, _) = aId == API.accountId authData
      (self, others) = List.partition isSelf . Map.toList $ allInfos

    allEvents <- traverse renderAccount' $ self <> others
    pure ( leftmost . map fst $ allEvents
         , leftmost . map snd $ allEvents
         )
  where
    renderAccount' (aId, infos) = do
        (renamed, deleted) <- renderAccount tz onlineStatus  authData aId infos
        pure (renamed, const aId <$> deleted)

-- Currently pretty dumb, once we have non anonymous accounts render those differently:
-- Visible group non devices belonging to a single account. Also family removal is per account.
renderAccount :: forall m t. (HasWebView m, MonadWidget t m)
              => TimeZone
              -> Dynamic t (Map DeviceId DeviceType)
              -> API.AuthData
              -> AccountId
              -> Dynamic t (Map DeviceId (Dynamic t (API.DeviceInfo)))
              -> m (Event t (DeviceId, Text), Event t ())
renderAccount tz onlineStatus authData accountId' infos = do
  let
    devName = fmap (fromMaybe "") . runMaybeT $ do
      inf <- MaybeT $ headMay . Map.elems <$> infos
      lift $ API.deviceInfoName <$> inf

    isUs = accountId' == API.accountId authData
  let
    deleteButton = el "td" $ do
      confirmationButton ("class" =: "btn btn-default")
        (
            elAttr "i" ( "class" =: "fa fa-fw fa-trash"
                        <> "data-toggle" =: "tooltip"
                        <> "data-placement" =: "right"
                        <> "title" =: "Remove from family"
                      ) blank
        )
        (dynText $ if isUs
                   then pure "This is you!\nReally leave your current family?"
                   else pure "Do you really want to remove device '" <> devName <> pure "' from the family?"
        )
  rs <- dyn $ renderRows <$> pure tz <*> infos <*> pure onlineStatus <*> pure authData <*> pure deleteButton
  (,) <$> switchPromptly never (fst <$> rs)
      <*> switchPromptly never (snd <$> rs)

renderRows :: forall m t. (HasWebView m, MonadWidget t m)
              => TimeZone
              -> Map DeviceId (Dynamic t (API.DeviceInfo))
              -> Dynamic t (Map DeviceId DeviceType)
              -> API.AuthData
              -> m (Event t ())
              -> m (Event t (DeviceId, Text), Event t ())
renderRows tz infos onlineStatus authData deleteColumn = do
    let
      isSelf (devId,_) = devId == API.deviceId authData
      (self, others) = List.partition isSelf . Map.toList $ infos

    selfEvents <- traverse (renderRow' True) self
    othersEvents <- traverse (renderRow' False) others
    let allEvents = selfEvents <> othersEvents
    pure (leftmost . map fst $ allEvents, leftmost . map snd $ allEvents)
  where
    renderRow' isSelf (devId, dynInfo) = do
        (renamed, deleted) <- renderRow tz isSelf onlineStatus deleteColumn devId dynInfo
        pure ((devId,) <$> renamed, deleted)


renderRow :: forall m t. (HasWebView m, MonadWidget t m)
              => TimeZone -> Bool -> Dynamic t (Map DeviceId DeviceType)
              -> m (Event t ())
              -> DeviceId -> Dynamic t API.DeviceInfo
              -> m (Event t Text, Event t ())
renderRow tz isSelf types deleteColumn devId devInfo = do
  let devType = Map.lookup devId <$> types
  elClass "tr" (if isSelf then "info" else "") $ do
    renderOnlineStatus devType
    -- elClass "td" "centered" $ blank
    el "td" $ dynText (API.deviceInfoName <$> devInfo)
    el "td" $ dynText (renderLocalTimeString . API.deviceInfoLastAccessed <$> devInfo)
    nameChanged <- el "td" $ do
      editStringButton ("class" =: "btn btn-default")
          ( elAttr "i" ( "class" =: "fa fa-fw fa-pencil"
                      <> "data-toggle" =: "tooltip"
                      <> "data-placement" =: "right"
                      <> "title" =: "edit device name"
                      ) blank
          )
          (text "Change device name to ...")
          (API.deviceInfoName <$> devInfo)
    (nameChanged,) <$> deleteColumn
  where
    renderLocalTimeString :: UTCTime -> Text
    renderLocalTimeString t =
      let
        locTime = utcToLocalTime tz t
      in
        T.pack $ formatTime defaultTimeLocale "%F %R" locTime

    renderOnlineStatus :: Dynamic t (Maybe DeviceType) -> m ()
    renderOnlineStatus dynDevType = do
      _ :: Event t () <- dyn $ renderOnlineStatus' <$> dynDevType
      pure ()

    renderOnlineStatus' :: Maybe DeviceType -> m ()
    renderOnlineStatus' Nothing
      = elClass "td" "centered" $ do
          elAttr "i" ( "class" =: "fa fa-circle-o"
                       <> "data-toggle" =: "tooltip"
                       <> "data-placement" =: "right"
                       <> "title" =: "offline"
                     ) blank
    renderOnlineStatus' (Just NoBaby)
      = elClass "td" "centered" $ do
          elAttr "i" ( "class" =: "fa fa-circle"
                       <> "style" =: "color:green;"
                       <> "data-toggle" =: "tooltip"
                       <> "data-placement" =: "right"
                       <> "title" =: "online"
                     ) blank
    renderOnlineStatus' (Just (Baby name))
      = elClass "td" "centered" $ do
          elAttr "i" ( "class" =: "fa fa-circle"
                       <> "style" =: "color:green;"
                       <> "data-toggle" =: "tooltip"
                       <> "data-placement" =: "right"
                       <> "title" =: "online as baby"
                     ) blank
          text name

