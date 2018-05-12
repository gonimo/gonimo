{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Gonimo.Client.Router.Impl.Native
Description : Routing for Gonimo.
Copyright   : (c) Robert Klotzner, 2018

Implementation for the Router interface that is not based on the browser's
history, but keeps track of the history by itself. This is needed for the native
Android app, as navigating the history there somehow breaks the app. (It seems
to reload.) I was not able to track down what exactly was going on there, but
luckily, thanks to this module, I no longer have to.
-}
module Gonimo.Client.Router.Impl.Native ( module Gonimo.Client.Router
                                        -- * Types
                                        , ModelConfig
                                        , HasModelConfig
                                        -- * Creation
                                         , make
                                         ) where

import qualified Data.Text                   as T
import qualified Language.Javascript.JSaddle as JS
import           Reflex.Dom.Core
import           Safe                        (headDef, tailSafe)

import           Gonimo.Client.Prelude
import           Gonimo.Client.Router
import qualified Gonimo.Client.Host         as Host
import           Gonimo.Client.Model


-- | Example datatype fulfilling 'HasModelConfig'.
type ModelConfig t = Host.Config t

-- | Configurations we provide for the model as inputs.
type HasModelConfig c t = (IsConfig c t, Host.HasConfig c)

-- | A JS variable we will set to true or false,
--
--   based on whether or not going back in history is possible (history stack is not empty).
canGoBackJSVar :: Text
canGoBackJSVar = "gonimoHistoryCanGoBack"

-- | Current route stored in JSVar - for debuggin purposes.
currentRouteJSVar :: Text
currentRouteJSVar = "gonimoHistoryCurrentRoute"

-- | JS Function one can call for telling Gonimo to go back one item in the history.
goBackJSFunc :: Text
goBackJSFunc = "gonimoHistoryGoBack"


make :: forall t m c mConf
        . (MonadWidget t m , HasConfig c, HasModelConfig mConf t)
      => c t -> m (mConf t, Router t)
make conf' = do
    conf <- breakCausalityLoop =<< handleJSGoBack
    history <- foldDyn id [] $ leftmost [ tailSafe <$  conf ^. onGoBack
                                        , (:)      <$> conf ^. onSetRoute
                                        ]
    let
      canGoBack = not . null <$> history

      onInvalidGoBack = fmap (const ()) . ffilter not . tag (current canGoBack) $ conf ^. onGoBack

      _route = headDef RouteHome <$> history

    tellJSAboutCanGoBack False
    performEvent_ $ tellJSAboutCanGoBack <$> updated canGoBack

    performEvent_ $ tellJSCurrentRoute <$> updated history

    let
      mConf = mempty & Host.onKillApp .~ onInvalidGoBack

    pure (mConf, Router {..})
  where
    tellJSAboutCanGoBack :: forall f. MonadJSM f => Bool -> f ()
    tellJSAboutCanGoBack = liftJSM . (JS.global JS.<# canGoBackJSVar)

    tellJSCurrentRoute :: forall f. MonadJSM f => [ Route ] -> f ()
    tellJSCurrentRoute = liftJSM . (JS.global JS.<# currentRouteJSVar) . T.pack . show

    handleJSGoBack :: m (c t)
    handleJSGoBack = do
      (onJSGoBack, triggerJSGoBack) <- newTriggerEvent
      jsCallBack <- liftJSM $ JS.function $ \_ _ _ -> liftIO (triggerJSGoBack ())
      liftJSM $ JS.global JS.<# goBackJSFunc $ jsCallBack
      pure $ conf' & onGoBack .~ leftmost [ conf' ^. onGoBack, onJSGoBack ]

    -- Reflex does not like it if screens which trigger route changes are shown based on routes.
    breakCausalityLoop c = do
      (onGoBack', triggerGoBack) <- newTriggerEvent
      (onSetRoute', triggerSetRoute) <- newTriggerEvent

      performEvent_ $ liftIO . triggerGoBack <$> c ^. onGoBack
      performEvent_ $ liftIO . triggerSetRoute <$> c ^. onSetRoute

      pure $ c & onGoBack .~ onGoBack'
               & onSetRoute .~ onSetRoute'


