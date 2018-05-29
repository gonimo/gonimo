{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RecursiveDo       #-}

{-|
Module      : Reflex.Dom.MDC.Internal
Description : Internal stuff for MDC components.
Copyright   : (c) Robert Klotzner, 2018
-}

module Reflex.Dom.MDC.Internal where

import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Language.Javascript.JSaddle (FromJSVal, JSM, MakeObject,
                                              MonadJSM, liftJSM)
import           Control.Monad.Fix
import           Control.Monad.IO.Class

import qualified Language.Javascript.JSaddle as JS
import           Reflex.Dom.Core
import           Safe                        (fromJustNote)


-- | Constraint needed by MDC components.
type MDCConstraint t m = ( Reflex t, MonadHold t m, MonadFix m, PerformEvent t m
                         , TriggerEvent t m, MonadIO (Performable m), PostBuild t m
                         , MonadJSM (Performable m), MonadJSM m, DomBuilder t m
                         , JS.ToJSVal (RawElement (DomBuilderSpace m))
                         )


-- | A line for separating content.
separator :: forall t m. DomBuilder t m => m ()
separator = elAttr "div" ("role" =: "separator" <> "class" =: "mdc-list-divider") blank

-- | Set an objects property to a given Haskell function.
setHaskellCallback :: (MakeObject this, MonadJSM m, MakeJSCallback f) => this -> Text -> f -> m ()
setHaskellCallback this propName f = liftJSM $ do
  let errMsg = "JavaScript value had wrong type in setHaskellCallback1. Concerned object property: " <> T.unpack propName
  fn <- makeJSCallback errMsg f
  this JS.<# propName $ fn


-- | Haskell functions that can be used for `setHaskellCallback`.
--
--   Note: This class has instances for JSM functions, if your function lives in
--   IO, use `liftIOJSM` instead of plain `liftIO` in order to help the type
--   checker pick the right instance.
class MakeJSCallback a where
  makeJSCallback :: String -> a -> JSM JS.Function

instance MakeJSCallback (JSM ()) where
  makeJSCallback _ m = JS.function $ \ _ _ [] -> m

instance FromJSVal a1 => MakeJSCallback (a1 -> JSM ()) where
  makeJSCallback msg f = JS.function $ \ _ _ [arg1] ->
    f =<< fromJustNote msg <$> JS.fromJSVal arg1

instance (FromJSVal a1, FromJSVal a2) => MakeJSCallback (a1 -> a2 -> JSM ()) where
  makeJSCallback msg f = JS.function $ \ _ _ [arg1, arg2] -> do
    a1 <- fromJustNote msg <$> JS.fromJSVal arg1
    a2 <- fromJustNote msg <$> JS.fromJSVal arg2
    f a1 a2

-- | Little helper for using MakeJSCallback
--
--   This helper can be used instead of a plain `liftIO` to help the type checker choose the right instance of `MakeJSCallback`.
liftIOJSM :: IO a -> JSM a
liftIOJSM = liftIO
