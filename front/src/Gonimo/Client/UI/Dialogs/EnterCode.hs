{-# LANGUAGE RecursiveDo #-}
{-|
Module      : Gonimo.Client.UI.Dialogs.EnterCode
Description : Dialog for entering an invitation code and thus accepting an invitation.
Copyright   : (c) Robert Klotzner, 2018
-}
module Gonimo.Client.UI.Dialogs.EnterCode where

import           Data.Map.Strict                      (Map)
import qualified Data.Text                            as T

import           Reflex.Dom.Core
import           Reflex.Dom.MDC.Dialog                (Dialog)
import qualified Reflex.Dom.MDC.Dialog                as Dialog



import qualified Gonimo.Client.Account                as Account
import qualified Gonimo.Client.Device                 as Device
import qualified Gonimo.Client.Family                 as Family
import           Gonimo.Client.Model                  (IsConfig)
import           Gonimo.Client.Prelude
import           Gonimo.Client.Reflex.Dom
import qualified Gonimo.Client.Settings               as Settings
import           Gonimo.Client.UI.Dialogs.EnterCode.I18N
import           Gonimo.I18N                          (i18n)




type HasModelConfig c t = (IsConfig c t, Account.HasConfig c, Device.HasConfig c)

type HasModel model = (Account.HasAccount model, Device.HasDevice model)


data Config t
  = Config { _onOpen  :: Event t ()
           , _onClose :: Event t ()
           }

ui :: forall model mConf m t. (HasModelConfig mConf t, HasModel model, GonimoM model t m)
  => Config t -> m (mConf t)
ui conf = mdo
  loc <- view Settings.locale
  model <- ask
  dialog <- Dialog.make
    $ Dialog.ConfigBase
    { Dialog._onOpen    = _onOpen conf
    , Dialog._onClose   = _onClose conf
    , Dialog._onDestroy = never
    , Dialog._header    = Dialog.HeaderHeading $ liftA2 i18n loc (pure Accept_Invitation)
    , Dialog._body      = do
        elClass "div" "invite-txt" $ do
          elClass "div" "mdc-text-field" $ do
            textInput $ def & textInputConfig_attributes .~ (pure $ "class" =: "mdc-text-field__input" <> "id" =: "invite-txt")
            elAttr "label" ("class" =: "mdc-text-field__label" <> "for" =: "invite-txt") blank
            elClass "div" "mdc-text-field__bottom-line" $ do
                trText Only_valid_for
                text $ " " <> (T.pack . show) Family.codeTimeout <> " "
                trText Seconds
    , Dialog._footer    = Dialog.cancelOnlyFooter $ liftA2 i18n loc (pure Cancel)
    }
  controller dialog

makeAnimationAttrs :: forall model m t. (HasModel model, GonimoM model t m)
  => Dialog t () -> Map Text Text -> m (Dynamic t (Map Text Text))
makeAnimationAttrs dialog staticAttrs = do
    fam <- view Device.selectedFamily
    let
      isOpen = dialog ^. Dialog.isOpen

      onAnimationEvents = updated $ isJust <$> fam ^. Family.activeInvitationCode

      onAnimationStart = gate (current isOpen) . ffilter_ id $ onAnimationEvents

      onAnimationEnd = leftmost [ ffilter_ not onAnimationEvents
                                , dialog ^. Dialog.onClosed
                                ]

    -- Small delay so the animation starts reliably.
    -- onAnimationStart <- delay 1 $ ffilter_ id onAnimationEvents

    foldDyn id staticAttrs $ leftmost [ addAnimAttrs <$ onAnimationStart
                                      , removeAnimAttrs <$ onAnimationEnd
                                      ]
  where
    removeAnimAttrs = (at "style" .~ Nothing) . removeClassAttr "anim"

    addAnimAttrs = addAnimDuration . addClassAttr "anim"

    addAnimDuration = at "style" .~ Just ("animation-duration: " <> animDurationText <> ";")

    animDurationText = (T.pack . show) Family.codeTimeout <> "s"


-- | "Business logic" of this screen.
--
--   Created a modelconfig based on the current state of affairs.
controller :: forall model mConf m t. (HasModelConfig mConf t, HasModel model, GonimoM model t m)
  => Dialog.Dialog t () -> m (mConf t)
controller dialog = pure mempty

-- Auto generated lenses ..

-- Lenses for Config t:

onOpen :: Lens' (Config t) (Event t ())
onOpen f config' = (\onOpen' -> config' { _onOpen = onOpen' }) <$> f (_onOpen config')

onClose :: Lens' (Config t) (Event t ())
onClose f config' = (\onClose' -> config' { _onClose = onClose' }) <$> f (_onClose config')


