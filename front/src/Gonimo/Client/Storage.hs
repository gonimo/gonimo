module Gonimo.Client.Storage (getItem
                             , setItem
                             , GHCJS.Storage) where

import qualified GHCJS.DOM.JSFFI.Generated.Storage as GHCJS
import Control.Monad.IO.Class (MonadIO)
import           GHCJS.DOM.Types (fromJSString, JSString, toJSString)
import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BL
import Control.Monad ((<=<))


getItem ::  (MonadIO m, ToJSON (key result), FromJSON result) => GHCJS.Storage -> key result -> m (Maybe result)
getItem storage key = (fromJsonString =<<) <$> GHCJS.getItem storage (toJsonString key)

setItem :: (MonadIO m, ToJSON (key data'), ToJSON data') => GHCJS.Storage -> key data' -> data' -> m ()
setItem storage key data' = GHCJS.setItem storage (toJsonString key) (toJsonString data')


toJsonString :: ToJSON a => a -> JSString
toJsonString = toJSString . T.decodeUtf8 . BL.toStrict . Aeson.encode

fromJsonString :: FromJSON a => JSString -> Maybe a
fromJsonString = Aeson.decodeStrict . T.encodeUtf8 . fromJSString
