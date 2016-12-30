{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

import Reflex
import Reflex.Dom hiding (webSocketConfig_send, webSocket_recv)
import qualified Data.Text as T
import Control.Lens
import Safe
import Control.Applicative
import Data.Maybe
import Data.Monoid
import Debug.Trace
import qualified Data.Map.Strict as Map
import qualified Gonimo.SocketAPI.Types as API
import qualified GHCJS.DOM.JSFFI.Generated.Storage as Storage
import qualified GHCJS.DOM.JSFFI.Generated.Window as Window
import Data.Default
import qualified Gonimo.Client.Server as Server
import Gonimo.Client.Server (webSocketConfig_send, webSocket_recv)
import qualified Gonimo.Client.Auth as Auth
import Gonimo.Client.Auth (AuthConfig(..), Auth, authConfigResponse, authRequest)



main :: IO ()
main = mainWidget $ mdo
  let wsConfig = def & webSocketConfig_send .~ serverRequests
  server <- Server.server "ws://localhost:8081" wsConfig
  let authConfig = AuthConfig { _authConfigResponse = server^.webSocket_recv }
  auth <- Auth.auth authConfig
  let serverRequests = (:[]) <$> auth^.authRequest
  pure ()

--   Storage.getItem 
-- mainWidget
--   $ el "div" $ do
--     el "ul" $ do
--       el "li" $ text "one"
--       el "li" $ text "two"
--       el "li" $ text "three"
--     a1 <- numberInput
--     d <- opInput
--     a2 <- numberInput
--     text "="
--     let values = (,) <$> a1 <*> a2
--     let r = zipDynWith (\op (a, b) -> doOp <$> pure op <*> a <*> b) (d^.dropdown_value) values
--     let strR = T.pack . maybe "" show <$> r
--     dynText strR

-- numberInput :: forall t m. MonadWidget t m => m (Dynamic t (Maybe Double))
-- numberInput = mdo
--         let
--           readDouble :: T.Text -> Maybe Double
--           readDouble = readMay . T.unpack


--         let
--           validState = "style" =: "border-color : blue"
--           errorState = "style" =: "border-color : red"

--         input <- textInput $ def & textInputConfig_inputType .~ "text"
--                                  & textInputConfig_initialValue .~ "0"
--                                  & textInputConfig_attributes .~ attrs

--         let result = readDouble <$> input^.textInput_value
--         let attrs = maybe errorState (const validState) <$> result
--         pure result

-- opInput :: forall t m. MonadWidget t m => m (Dropdown t ArithOp)
-- opInput = do
--    let ops = [ Plus, Minus, Mult, Division]
--    let strOps = map (T.pack . show) ops
--    let opMap = Map.fromList $ zip ops strOps
--    dropdown Plus (constDyn opMap) def
-- 1c50bdd928..da70d3da0f
