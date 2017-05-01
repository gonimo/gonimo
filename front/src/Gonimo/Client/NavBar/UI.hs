module Gonimo.Client.NavBar.UI where

import           Control.Lens
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import qualified Gonimo.Client.App.Types           as App
import qualified Gonimo.Client.DeviceList.Internal as DeviceList
import qualified Gonimo.Client.Family.Internal     as Family
import           Gonimo.Client.NavBar.Internal
import           Gonimo.Client.Reflex.Dom
import           Reflex.Dom.Core
import           Data.Monoid
import           Gonimo.Client.EditStringButton (editDeviceName, editFamilyName)
import qualified Gonimo.SocketAPI as API
import qualified Gonimo.SocketAPI.Types as API
import Gonimo.Client.Prelude


navBar :: forall m t. GonimoM t m
      => Config t -> m (NavBar t)
navBar config = do
    let loaded = config^.configLoaded
    let deviceList = config^.configDeviceList
    let deviceName = DeviceList.ownDeviceName (loaded^.App.authData) deviceList
    let cFamilyName = Family.currentFamilyName
                      $ Family.DefiniteFamily (loaded^.App.families) (loaded^.App.selectedFamily)
    (backClicked', homeClicked', devNameChangeReq, famNameChangeReq) <-
      elClass "div" "menu" $ do
          backClicked' <- backButton
          homeClicked' <- homeButton
          (devNameChangeReq, famNameChangeReq) <-
            elClass "div" "menu-right" $ do
              devNameChangeReq <-
                makeClickable . elAttr' "span" (addBtnAttrs "device-self") . dynText
                $ makeEllipsis 20 <$> deviceName
              el "br" blank
              famNameChangeReq <-
                makeClickable . elAttr' "span" ("role" =: "button" <> "type" =: "button") . dynText
                $ makeEllipsis 20 <$> cFamilyName
              pure (devNameChangeReq, famNameChangeReq)

          gonimoClicked <- makeClickable .  elAttr' "div" (addBtnAttrs "menu-center") $ blank
          pure $ (backClicked', (leftmost [homeClicked', gonimoClicked]), devNameChangeReq, famNameChangeReq)

    devNameChanged <- editDeviceName (pure devNameChangeReq) deviceName
    famNameChanged <- editFamilyName (pure famNameChangeReq) cFamilyName
    let deviceId = API.deviceId <$> config^.configLoaded.App.authData
    let familyId = config^.configLoaded.App.selectedFamily
    let setDevName = current $ API.ReqSetDeviceName <$> deviceId
    let setFamName = current $ API.ReqSetFamilyName <$> familyId
    let request' = mconcat . map (fmap (:[])) $ [ attachWith ($) setDevName devNameChanged
                                                , attachWith ($) setFamName famNameChanged
                                                ]
    pure $ NavBar { _backClicked = backClicked'
                  , _homeClicked = homeClicked'
                  , _request = request'
                  }
  where
    backButton = makeClickable . elAttr' "div" (addBtnAttrs "menu-left back") $ blank

    homeButton = makeClickable . elAttr' "div" (addBtnAttrs "menu-left home") $ blank

makeEllipsis :: Int -> Text -> Text
makeEllipsis maxL t = if T.length t > maxL
                      then T.take (maxL-2) t <> ".."
                      else t
