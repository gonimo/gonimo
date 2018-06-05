module Gonimo.Client.Storage ( getItemLocal
                             , setItemLocal
                             , getItem
                             , setItem
                             , GHCJS.Storage) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson             (FromJSON, ToJSON)
import qualified Data.Aeson             as Aeson
import qualified Data.ByteString.Lazy   as BL
import qualified Data.Text.Encoding     as T
import qualified GHCJS.DOM              as DOM
import qualified GHCJS.DOM.Storage      as GHCJS
import           GHCJS.DOM.Types        (JSString, MonadJSM, fromJSString,
                                         toJSString)
import qualified GHCJS.DOM.Window       as Window


-- | Retrieve data from local storage.
getItemLocal ::  (MonadIO m, ToJSON (key result), FromJSON result, MonadJSM m) => key result -> m (Maybe result)
getItemLocal key = do
    storage <- Window.getLocalStorage =<< DOM.currentWindowUnchecked
    getItem storage key

-- | Write data to local storage.
setItemLocal :: (MonadIO m, ToJSON (key data'), ToJSON data', MonadJSM m) => key data' -> data' -> m ()
setItemLocal key val = do
    storage <- Window.getLocalStorage =<< DOM.currentWindowUnchecked
    setItem storage key val

getItem ::  (MonadIO m, ToJSON (key result), FromJSON result, MonadJSM m) => GHCJS.Storage -> key result -> m (Maybe result)
getItem storage key = (fromJsonString =<<) <$> GHCJS.getItem storage (toJsonString key)

setItem :: (MonadIO m, ToJSON (key data'), ToJSON data', MonadJSM m) => GHCJS.Storage -> key data' -> data' -> m ()
setItem storage key data' = GHCJS.setItem storage (toJsonString key) (toJsonString data')


toJsonString :: ToJSON a => a -> JSString
toJsonString = toJSString . T.decodeUtf8 . BL.toStrict . Aeson.encode

fromJsonString :: FromJSON a => JSString -> Maybe a
fromJsonString = Aeson.decodeStrict . T.encodeUtf8 . fromJSString
