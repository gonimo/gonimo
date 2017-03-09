module Gonimo.Client.NavBar.UI where

import           Control.Lens
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import qualified Gonimo.Client.App.Types           as App
import           Gonimo.Client.ConfirmationButton  (confirmationEl)
import qualified Gonimo.Client.DeviceList.Internal as DeviceList
import qualified Gonimo.Client.Family.Internal     as Family
import           Gonimo.Client.NavBar.Internal
import           Gonimo.Client.Reflex.Dom
import           Reflex.Dom.Core
import           Data.Monoid


navBar :: forall m t. (HasWebView m, MonadWidget t m)
      => Config t -> m (NavBar t)
navBar config = do
    let loaded = config^.configLoaded
    let deviceList = config^.configDeviceList
    elClass "div" "menu" $ do
        backClicked' <- backButton
        homeClicked' <- homeButton
        elClass "div" "menu-right" $ do
          let deviceName = DeviceList.ownDeviceName (loaded^.App.authData) deviceList
          dynText $ makeEllipsis 20 <$> deviceName
          el "br" blank
          let cFamilyName = Family.currentFamilyName
                            $ Family.DefiniteFamily (loaded^.App.families) (loaded^.App.selectedFamily)
          dynText $ makeEllipsis 20 <$> cFamilyName
        gonimoClicked <- makeClickable .  elAttr' "div" (addBtnAttrs "menu-center") $ blank
        pure $ NavBar backClicked' (leftmost [homeClicked', gonimoClicked])
  where
    backButton = makeClickable . elAttr' "div" (addBtnAttrs "menu-left back") $ blank

    homeButton = makeClickable . elAttr' "div" (addBtnAttrs "menu-left home") $ blank

makeEllipsis :: Int -> Text -> Text
makeEllipsis maxL t = if T.length t > maxL
                      then T.take (maxL-2) t <> ".."
                      else t
