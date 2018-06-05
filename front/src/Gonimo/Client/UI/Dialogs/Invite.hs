{-|
Module      : Gonimo.Client.UI.Dialogs.Invite
Description : Dialogs for showing invitation code and sending invitations.
Copyright   : (c) Robert Klotzner, 2018
-}
module Gonimo.Client.UI.Dialogs.Invite where

import           Reflex.Dom.Core
import qualified Reflex.Dom.MDC.Dialog                as Dialog


import           Gonimo.Client.Model                  (IsConfig)
import           Gonimo.Client.Prelude

import qualified Gonimo.Client.Account                as Account
import qualified Gonimo.Client.Device                 as Device
import qualified Gonimo.Client.Family                 as Family
import qualified Gonimo.Client.Settings               as Settings
import           Gonimo.Client.UI.Dialogs.Invite.I18N
import           Gonimo.I18N                          (i18n)
import qualified Gonimo.SocketAPI.Types               as API



type HasModelConfig c t = (IsConfig c t, Account.HasConfig c, Device.HasConfig c)

type HasModel model = (Account.HasAccount model, Device.HasDevice model)


data Config t
  = Config { _onOpen  :: Event t ()
           , _onClose :: Event t ()
           }

ui :: forall model mConf m t. (HasModelConfig mConf t, HasModel model, GonimoM model t m)
  => Config t -> m (mConf t)
ui conf = do
  loc <- view Settings.locale
  model <- ask
  void . Dialog.make
    $ Dialog.ConfigBase
    { Dialog._onOpen    = _onOpen conf
    , Dialog._onClose   = _onClose conf
    , Dialog._onDestroy = never
    , Dialog._header    = Dialog.HeaderHeading $ liftA2 i18n loc (pure Invitation_Code)
    , Dialog._body      = do
        elClass "div" "code-txt" $ do
          elClass "h2" "code-field" $ dynText (showCode model)
          elAttr "div" ("role" =: "progressbar" <> "class" =: "mdc-linear-progress") $ do
            elClass "div" "mdc-linear-progress__bar mdc-linear-progress__primary-bar"  $ do
              elClass "span" "mdc-linear-progress__bar-inner" blank
          elAttr "p" ("class" =: "mdc-text-field-helper-text--persistent" <> "aria-hidden" =: "true") $ do
            elAttr "i" ("class" =: "material-icons" <> "aria-hidden" =: "true") $ text "schedule"
            elClass "span" "code-time" $ text "0:30"
          elClass "div" "mdc-menu-anchor" $ do
            elAttr "button" ("type" =: "button" <> "class" =: "mdc-button mdc-button--flat btn share-btn") $ do
              elAttr "i" ("class" =: "material-icons" <> "aria-hidden" =: "true") $ text "share"
              text "Teilen"
            elAttr "div" ("class" =: "mdc-simple-menu" <> "tabindex" =: "-1") $ do
              elAttr "ul" ("class" =: "mdc-simple-menu__items mdc-list" <> "role" =: "menu" <> "aria-hidden" =: "true") $ do
                elAttr "li" ("class" =: "mdc-list-item" <> "role" =: "menuitem" <> "tabindex" =: "0") $ do
                  elClass "i" "material-icons" $ text "mail"
                  text "Mail"
                elAttr "li" ("class" =: "mdc-list-item" <> "role" =: "menuitem" <> "tabindex" =: "0") $ do
                  elClass "i" "material-icons" $ text "content_copy"
                  text "Kopieren"
        el "br" blank
        Dialog.separator
        el "br" blank
    , Dialog._footer    = Dialog.cancelOnlyFooter $ liftA2 i18n loc (pure Cancel)
    }
  controller conf

showCode :: (Reflex t, Device.HasDevice model, Settings.HasSettings model)
  => model t -> Dynamic t Text
showCode model = do
  let loc = model ^. Settings.locale
  mCode <- runMaybeT $ do
    fam <- MaybeT $ model ^. Device.selectedFamily
    MaybeT $ fam ^. Family.activeInvitationCode
  loadingStr <- liftA2 i18n loc (pure Loading)
  pure $ maybe loadingStr API.codeToText $ mCode

-- showCode :: (Reflex t, Device.HasDevice model, Settings.HasSettings model)
--   => model t -> Dynamic t Text
-- showCode model = do
--   let loc = model ^. Settings.locale
--   mCode <- runMaybeT $ do
--     fam <- MaybeT $ model ^. Device.selectedFamily
--     pure $ fam ^. Family.identifier
--   loadingStr <- liftA2 i18n loc (pure Loading)
--   pure $ maybe loadingStr (T.pack . show) $ mCode

controller :: forall model mConf m t. (HasModelConfig mConf t, HasModel model, GonimoM model t m)
  => Config t -> m (mConf t)
controller conf = do
  model <- ask
  onCreateFamily     <- Account.ifFamiliesEmpty model $ conf ^. onOpen
  onCreateInvitation <- Device.filterWithFamily model Family.ifNoActiveInvitation $ conf ^.onOpen
  onCreateCodeStart  <- Device.filterWithFamily model Family.ifNoActiveInvitationCode $ conf ^. onOpen

  isOpen <- hold False $ leftmost [ True <$ conf ^. onOpen
                                  , False <$ conf ^. onClose
                                  ]

  let
    onCodeInvalid = ffilter_ id
                    . updated $ do
                        mFam <- model ^. Device.selectedFamily
                        case mFam of
                          Nothing  -> pure False
                          Just fam -> isNothing <$> fam ^. Family.activeInvitationCode

    onCreateCodeReload = fmap (const ()) . ffilter id . tag isOpen $ onCodeInvalid

  let
    famConf = mempty & Family.onCreateInvitation .~ onCreateInvitation
                     & Family.onCreateCode .~ leftmost [ onCreateCodeStart
                                                       , onCreateCodeReload
                                                       ]
  pure $ mempty & Account.onCreateFamily .~ onCreateFamily
                & Device.familyConfig .~ famConf

-- Auto generated lenses ..

-- Lenses for Config t:

onOpen :: Lens' (Config t) (Event t ())
onOpen f config' = (\onOpen' -> config' { _onOpen = onOpen' }) <$> f (_onOpen config')

onClose :: Lens' (Config t) (Event t ())
onClose f config' = (\onClose' -> config' { _onClose = onClose' }) <$> f (_onClose config')


