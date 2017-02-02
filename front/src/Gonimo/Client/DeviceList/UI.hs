{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Gonimo.Client.DeviceList.UI where

import Reflex.Dom
import Control.Monad
import Data.Monoid
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
import Data.Maybe (fromMaybe)
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

-- Overrides configCreateDeviceList && configLeaveDeviceList
ui :: forall m t. (HasWebView m, MonadWidget t m)
            => Config t -> m (DeviceList t)
ui config = mdo
    deviceList' <- deviceList config
    renderList config deviceList'
    pure deviceList'

renderList :: forall m t. (HasWebView m, MonadWidget t m)
              => Config t -> DeviceList t
              -> m ()
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
    el "tbody" $ dyn $ renderRows <$> deviceList'^.deviceIds
                                  <*> deviceList'^.deviceInfos
                                  <*> pure (deviceList'^.onlineDevices) -- don't re-render full table
                                  <*> config^.configAuthData
  pure ()


renderRows :: forall m t. (HasWebView m, MonadWidget t m)
              => Map AccountId (Dynamic t [DeviceId])
              -> Map DeviceId (Dynamic t (API.DeviceInfo))
              -> Dynamic t (Map DeviceId DeviceType)
              -> Maybe API.AuthData
              -> m ()
renderRows deviceIds' infos onlineStatus mAuthData = do
  tz <- liftIO $ getCurrentTimeZone
  let
    isSelf (devId, info') = Just devId == (API.deviceId <$> mAuthData)
    (self, others) = List.partition isSelf . Map.toList $ infos
  traverse_ (uncurry (renderRow tz "info" onlineStatus)) self
  traverse_ (uncurry (renderRow tz "" onlineStatus)) others

renderRow :: forall m t. (HasWebView m, MonadWidget t m)
              => TimeZone -> Text -> Dynamic t (Map DeviceId DeviceType)
              -> DeviceId -> Dynamic t API.DeviceInfo
              -> m ()
renderRow tz rowClass types devId devInfo = do
  let devType = Map.lookup devId <$> types
  elClass "tr" rowClass $ do
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
    el "td" $ do
      elAttr "i" ( "class" =: "fa fa-fw fa-trash"
                   <> "data-toggle" =: "tooltip"
                   <> "data-placement" =: "right"
                   <> "title" =: "Remove from family"
                 ) blank
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

