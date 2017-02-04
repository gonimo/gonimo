{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
module Gonimo.Client.DeviceList.UI where

import Reflex.Dom
import Control.Monad
import Data.Monoid
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Safe (headMay)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import Gonimo.Db.Entities (DeviceId, AccountId)
import qualified Gonimo.Db.Entities as Db
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Gonimo.SocketAPI.Types as API
import qualified Gonimo.SocketAPI as API
import Control.Lens
import qualified GHCJS.DOM.JSFFI.Generated.Window as Window
import qualified Gonimo.Client.Storage as GStorage
import qualified Gonimo.Client.Storage.Keys as GStorage
import qualified GHCJS.DOM as DOM
import Data.Foldable (traverse_)
import Gonimo.Client.Reflex
import Data.Maybe (fromMaybe, catMaybes)
import qualified Gonimo.Types as Gonimo
import qualified Gonimo.Client.Invite as Invite
import Control.Monad.IO.Class (liftIO)
import Unsafe.Coerce
import Debug.Trace (trace)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime, TimeZone)
import Data.Time.Clock (UTCTime)
import Gonimo.Types (DeviceType(..))

import Gonimo.Client.DeviceList.Internal
import Gonimo.Client.ConfirmationButton (confirmationButton)

-- Overrides configCreateDeviceList && configLeaveDeviceList
ui :: forall m t. (HasWebView m, MonadWidget t m)
            => Config t -> m (DeviceList t)
ui config = mdo
    deviceList' <- deviceList config
    (renameEv, removeEv) <- renderList config deviceList'
    let renameReq = uncurry API.ReqSetDeviceName <$> renameEv
    let removeReq = uncurry (flip API.ReqLeaveFamily)
                    <$> attach (current (config^.configFamilyId)) removeEv
    pure $ deviceList' & request %~ ( mconcat (fmap (:[]) <$> [renameReq, removeReq]) <>)

renderList :: forall m t. (HasWebView m, MonadWidget t m)
              => Config t -> DeviceList t
              -> m (Event t (DeviceId, Text), Event t AccountId)
renderList config deviceList' = do
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
    evEvents <- el "tbody" $ dyn $ renderAccounts tz
                <$> deviceList'^.deviceIds
                <*> deviceList'^.deviceInfos
                <*> pure (deviceList'^.onlineDevices) -- don't re-render full table
                <*> config^.configAuthData

    (,) <$> switchPromptly never (fst <$> evEvents)
        <*> switchPromptly never (snd <$> evEvents)

renderAccounts :: forall m t. (HasWebView m, MonadWidget t m)
              => TimeZone
              -> Map AccountId (Dynamic t [DeviceId])
              -> Map DeviceId (Dynamic t (API.DeviceInfo))
              -> Dynamic t (Map DeviceId DeviceType)
              -> Maybe API.AuthData
              -> m (Event t (DeviceId, Text), Event t AccountId)
renderAccounts tz deviceIds' infos onlineStatus mAuthData = do
    let
      isSelf (aId, info') = Just aId == (API.accountId <$> mAuthData)
      (self, others) = List.partition isSelf . Map.toList $ deviceIds'

    allEvents <- traverse renderAccount' $ self <> others
    pure ( leftmost . map fst $ allEvents
         , leftmost . map snd $ allEvents
         )
  where
    renderAccount' (aId, devIds) = do
        (renamed, deleted) <- renderAccount tz infos onlineStatus  mAuthData aId devIds
        pure (renamed, const aId <$> deleted)

-- Currently pretty dumb, once we have non anonymous accounts render those differently:
-- Visible group non devices belonging to a single account. Also family removal is per account.
renderAccount :: forall m t. (HasWebView m, MonadWidget t m)
              => TimeZone
              -> Map DeviceId (Dynamic t (API.DeviceInfo))
              -> Dynamic t (Map DeviceId DeviceType)
              -> Maybe API.AuthData
              -> AccountId
              -> Dynamic t [DeviceId]
              -> m (Event t (DeviceId, Text), Event t ())
renderAccount tz infos onlineStatus mAuthData  accountId' devIds'= do
  let
    filterInfos infos' devIds =
      let
        mMapValues = flip Map.lookup infos' <$> devIds
        mEntries = zipWith (\k mv -> (k,) <$> mv) devIds mMapValues
      in
        Map.fromList . catMaybes $ mEntries
    filteredInfos = filterInfos infos <$> devIds'


  let
    devName = fmap (fromMaybe "") . runMaybeT $ do
      myDevId <- MaybeT $ headMay <$> devIds'
      MaybeT . sequence $ infos^?at myDevId._Just.to (fmap API.deviceInfoName)
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
        (dynText $ pure "Do you really want to remove device '" <> devName <> pure "'?")
  rs <- dyn $ renderRows <$> pure tz <*> filteredInfos <*> pure onlineStatus <*> pure mAuthData <*> pure deleteButton
  (,) <$> switchPromptly never (fst <$> rs)
      <*> switchPromptly never (snd <$> rs)

renderRows :: forall m t. (HasWebView m, MonadWidget t m)
              => TimeZone
              -> Map DeviceId (Dynamic t (API.DeviceInfo))
              -> Dynamic t (Map DeviceId DeviceType)
              -> Maybe API.AuthData
              -> m (Event t ())
              -> m (Event t (DeviceId, Text), Event t ())
renderRows tz infos onlineStatus mAuthData deleteColumn = do
    let
      isSelf (devId, info') = Just devId == (API.deviceId <$> mAuthData)
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
    el "td" $ do
      elAttr "i" ( "class" =: "fa fa-fw fa-pencil"
                   <> "data-toggle" =: "tooltip"
                   <> "data-placement" =: "right"
                   <> "title" =: "edit device name"
                 ) blank
    (never,) <$> deleteColumn
  where
    renderLocalTimeString :: UTCTime -> Text
    renderLocalTimeString t =
      let
        locTime = utcToLocalTime tz t
      in
        T.pack $ formatTime defaultTimeLocale "%F %R" locTime

    renderOnlineStatus :: Dynamic t (Maybe DeviceType) -> m ()
    renderOnlineStatus dynDevType = do
      dyn $ renderOnlineStatus' <$> dynDevType
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

