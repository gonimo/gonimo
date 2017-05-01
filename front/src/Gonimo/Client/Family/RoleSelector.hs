{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Gonimo.Client.Family.RoleSelector where

import           Reflex.Dom.Core

import           Gonimo.Client.Family.Internal
import           Gonimo.Client.Reflex.Dom
import           Gonimo.Client.Prelude
import           Gonimo.Client.Util
import           Gonimo.Client.Family.RoleSelector.I18N
import           Control.Monad.Reader.Class



roleSelector :: forall t m. (MonadReader (GonimoEnv t) m, MonadFix m, DomBuilder t m, MonadHold t m, PostBuild t m)
          => m (Event t GonimoRole)
roleSelector = do
  elClass "div" "btn-box" $ do
    babyClicked <-
      makeClickable . elAttr' "div" (addFullScreenBtnAttrs "btn-baby") $ do
        elAttr "img" ("src" =: "/pix/button-baby.svg") blank
        el "span" $ trText Baby
    parentClicked <-
      makeClickable . elAttr' "div" (addFullScreenBtnAttrs "btn-parent") $ do
        elAttr "img" ("src" =: "/pix/button-parent.svg") blank
        el "span" $ trText Parent
    pure $ leftmost [ const RoleBaby <$> babyClicked
                    , const RoleParent <$> parentClicked
                    ]
