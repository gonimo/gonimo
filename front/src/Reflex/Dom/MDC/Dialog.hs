{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RecursiveDo       #-}

{-|
Module      : Reflex.Dom.MDC.Dialog
Description : Material Design Dialog component.
Copyright   : (c) Robert Klotzner, 2018
-}

module Reflex.Dom.MDC.Dialog ( -- * Types and Classes
                               ConfigBase(..)
                             , HasConfigBase(..)
                             , Config
                             , Dialog(..)
                             , HasDialog(..)
                             , HeaderBase(..)
                             , Header
                             -- * Creation
                             , make
                             -- * Utilities
                             , cancelOnlyFooter
                             , separator
                             ) where


import           Control.Lens

import           Data.Map                    (Map)
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Language.Javascript.JSaddle (liftJSM)
import           Data.List                   (delete)
import qualified Language.Javascript.JSaddle as JS
import           Reflex.Dom.Core
import           Control.Monad


import           Reflex.Dom.MDC.Internal


-- | Make text a type parameter for easy I18N. All text is wrapped in `Dynamic`, so the language can be changed at runtime.
data ConfigBase text t m a
  = ConfigBase { _onOpen    :: Event t ()
               , _onClose   :: Event t ()
               , _onDestroy :: Event t ()
               -- , _AriaLabel       :: Dynamic t text -- We will take care of aria stuff at some later point in time properly.
               -- , _AriaDescription :: Dynamic t text
               , _header    :: HeaderBase text t m
               , _body      :: m a
               , _footer    :: m ()
               }


data HeaderBase text t m = HeaderHeading (Dynamic t text)
                               | HeaderBare (m ())

type Config t m a = ConfigBase Text t m a

type Header t m = HeaderBase Text t m

data Dialog t a
  = Dialog { _onAccepted :: Event t ()
           , _onCanceled :: Event t ()
             -- | Just a forwarded `onOpen` for your convenience.
           , _onOpened :: Event t ()
           , _isOpen :: Dynamic t Bool
           , _result     :: a
           }

make :: forall t m a. MDCConstraint t m => Config t m a -> m (Dialog t a)
make conf = mdo
    (onAddClass, triggerAddClass)       <- newTriggerEvent
    (onRemoveClass, triggerRemoveClass) <- newTriggerEvent

    (_onCanceled, triggerCancel) <- newTriggerEvent
    (_onAccepted, triggerAccept) <- newTriggerEvent

    (dialogTag, (surfaceTag, _result)) <- html conf dynAttrs

    -- make adapter with JS prototype.
    constr  <- liftJSM $ JS.eval ("window.reflexDomMDC.Dialog" :: Text)
    adapter <- liftJSM $ JS.new constr [ _element_raw dialogTag
                                       , _element_raw surfaceTag
                                       ]

    setHaskellCallback adapter "addClass"     (liftIOJSM . triggerAddClass)
    setHaskellCallback adapter "removeClass"  (liftIOJSM . triggerRemoveClass)
    setHaskellCallback adapter "notifyCancel" (liftIOJSM $ triggerCancel ())
    setHaskellCallback adapter "notifyAccept" (liftIOJSM $ triggerAccept ())

    let -- Foundation functions to call:
      jsOpen    = liftJSM . void $ adapter ^. JS.js0 ("open" :: Text)
      jsClose   = liftJSM . void $ adapter ^. JS.js0 ("close" ::Text)
      jsDestroy = liftJSM . void $ adapter ^. JS.js0 ("destroy" ::Text)

      _onOpened = conf ^. onOpen

    performEvent_ $ jsOpen    <$ conf ^. onOpen
    performEvent_ $ jsClose   <$ conf ^. onClose
    performEvent_ $ jsDestroy <$ conf ^. onDestroy

    dynAttrs <- foldDyn id staticAttrs $ leftmost [ addClassAttr <$> onAddClass
                                                  , removeClassAttr <$> onRemoveClass
                                                  ]
    _isOpen <- holdDyn False $ leftmost [ True <$ conf ^. onOpen
                                        , False <$ _onCanceled
                                        , False <$ _onAccepted
                                        ]

    pure $ Dialog {..}

  where
    staticAttrs = "class" =: "mdc-dialog" <> "role" =: "alertdialog"


    addClassAttr :: Text -> Map Text Text -> Map Text Text
    addClassAttr className = at "class" . non T.empty %~ addClass className

    removeClassAttr :: Text -> Map Text Text -> Map Text Text
    removeClassAttr className = at "class" . non T.empty %~ removeClass className

    addClass :: Text -> Text -> Text
    addClass className classes = classes <> " " <> className

    removeClass :: Text -> Text -> Text
    removeClass className classes = T.unwords . delete className . T.words $ classes


-- | Simple footer, consisting only of a Cancel button.
--
--   The given `Dynamic` `Text` will be the label of the button. Applied to a
--   `Dynamic` `Text` this can be used as `_footer`.
cancelOnlyFooter :: MDCConstraint t m => Dynamic t Text -> m ()
cancelOnlyFooter btnText = elAttr "button" ("type" =: "button" <> "class" =: "mdc-button mdc-dialog__footer__button mdc-dialog__footer__button--cancel") $ dynText btnText


-- | Little helper for cleaning up the type signature of `html`.
type ElementTuple t m a = (Element EventResult (DomBuilderSpace m) t, a)

html :: forall t m a. (DomBuilder t m, PostBuild t m)
           => Config t m a -> Dynamic t (Map Text Text) -> m (ElementTuple t m (ElementTuple t m a))
html conf dynAttrs = do
    r <-
      elDynAttr' "aside" dynAttrs $ do
        elClass' "div" "mdc-dialog__surface" $ do
          elClass "header" "mdc-dialog__header" $
            renderHeader $ conf ^. header
          a <-
            elClass "section" "mdc-dialog__body" $ conf ^. body
          elClass "footer" "mdc-dialog__footer" $ conf ^. footer
          pure a
    elClass "div" "mdc-dialog__backdrop" blank
    pure r
  where
    renderHeader :: Header t m -> m ()
    renderHeader (HeaderHeading dt) = elClass "h2" "mdc-dialog__header__title" $ dynText dt
    renderHeader (HeaderBare m)     = m


testDialog :: forall t m. MonadWidget t m => m (Dialog t ())
testDialog = make $ ConfigBase { _onOpen = never
                               , _onClose = never
                               , _onDestroy = never
                               , _header = HeaderHeading (pure "hhuhu")
                               , _body = separator
                               , _footer = separator
                               }
-- Auto generated lenses:
class HasConfigBase a42 where
  configBase :: Lens' (a42 text t m a) (ConfigBase text t m a)

  onOpen :: Lens' (a42 text t m a) (Event t ())
  onOpen = configBase . go
    where
      go :: Lens' (ConfigBase text t m a) (Event t ())
      go f configBase' = (\onOpen' -> configBase' { _onOpen = onOpen' }) <$> f (_onOpen configBase')


  onClose :: Lens' (a42 text t m a) (Event t ())
  onClose = configBase . go
    where
      go :: Lens' (ConfigBase text t m a) (Event t ())
      go f configBase' = (\onClose' -> configBase' { _onClose = onClose' }) <$> f (_onClose configBase')


  onDestroy :: Lens' (a42 text t m a) (Event t ())
  onDestroy = configBase . go
    where
      go :: Lens' (ConfigBase text t m a) (Event t ())
      go f configBase' = (\onDestroy' -> configBase' { _onDestroy = onDestroy' }) <$> f (_onDestroy configBase')


  header :: Lens' (a42 text t m a) (HeaderBase text t m)
  header = configBase . go
    where
      go :: Lens' (ConfigBase text t m a) (HeaderBase text t m)
      go f configBase' = (\header' -> configBase' { _header = header' }) <$> f (_header configBase')


  body :: Lens' (a42 text t m a) (m a)
  body = configBase . go
    where
      go :: Lens' (ConfigBase text t m a) (m a)
      go f configBase' = (\body' -> configBase' { _body = body' }) <$> f (_body configBase')


  footer :: Lens' (a42 text t m a) (m ())
  footer = configBase . go
    where
      go :: Lens' (ConfigBase text t m a) (m ())
      go f configBase' = (\footer' -> configBase' { _footer = footer' }) <$> f (_footer configBase')


instance HasConfigBase ConfigBase where
  configBase = id


class HasDialog a42 where
  dialog :: Lens' (a42 t a) (Dialog t a)

  onAccepted :: Lens' (a42 t a) (Event t ())
  onAccepted = dialog . go
    where
      go :: Lens' (Dialog t a) (Event t ())
      go f dialog' = (\onAccepted' -> dialog' { _onAccepted = onAccepted' }) <$> f (_onAccepted dialog')


  onCanceled :: Lens' (a42 t a) (Event t ())
  onCanceled = dialog . go
    where
      go :: Lens' (Dialog t a) (Event t ())
      go f dialog' = (\onCanceled' -> dialog' { _onCanceled = onCanceled' }) <$> f (_onCanceled dialog')


  onOpened :: Lens' (a42 t a) (Event t ())
  onOpened = dialog . go
    where
      go :: Lens' (Dialog t a) (Event t ())
      go f dialog' = (\onOpened' -> dialog' { _onOpened = onOpened' }) <$> f (_onOpened dialog')


  isOpen :: Lens' (a42 t a) (Dynamic t Bool)
  isOpen = dialog . go
    where
      go :: Lens' (Dialog t a) (Dynamic t Bool)
      go f dialog' = (\isOpen' -> dialog' { _isOpen = isOpen' }) <$> f (_isOpen dialog')


  result :: Lens' (a42 t a) a
  result = dialog . go
    where
      go :: Lens' (Dialog t a) a
      go f dialog' = (\result' -> dialog' { _result = result' }) <$> f (_result dialog')


instance HasDialog Dialog where
  dialog = id

