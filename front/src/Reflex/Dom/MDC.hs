{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RecursiveDo       #-}

{-|
Module      : Reflex.Dom.MDC
Description : Material Design Components for Reflex.
Copyright   : (c) Robert Klotzner, 2018

This file exports basic and simple components. Bigger components like
"Reflex.Dom.MDC.Dialog" are designed to be imported qualified and thus have to
be imported separately.
-}

module Reflex.Dom.MDC where


import           Control.Lens
import           Control.Monad.IO.Class      (liftIO)
import           Data.Map                    (Map)
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Language.Javascript.JSaddle (FromJSVal, JSM, JSVal, MakeObject,
                                              MonadJSM, liftJSM)

import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Data.List                   (delete)
import qualified Language.Javascript.JSaddle as JS
import           Reflex.Dom.Core
import           Safe                        (fromJustNote)


mdcButton :: DomBuilder t1 m => t -> m a -> m a
mdcButton classes = elClass "button" "mdc-button mdc-buttonj"

