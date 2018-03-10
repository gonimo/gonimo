module Gonimo.Client.Family.RoleSelector where

import           Reflex.Dom.Core

import           Gonimo.Client.Family.Internal
import           Gonimo.Client.Reflex.Dom
import           Gonimo.Client.Prelude
import           Gonimo.Client.Family.RoleSelector.I18N

roleSelector :: forall model t m. GonimoM model t m => m (Event t GonimoRole)
roleSelector = do
  elClass "div" "btn-box" $ do
    babyClicked <-
      makeClickable . elAttr' "div" (addBtnAttrs "btn-baby") $ do
        elAttr "img" ("src" =: "/pix/button-baby.svg") blank
        el "span" $ trText Baby
    parentClicked <-
      makeClickable . elAttr' "div" (addBtnAttrs "btn-parent") $ do
        elAttr "img" ("src" =: "/pix/button-parent.svg") blank
        el "span" $ trText Parent
    pure $ leftmost [ RoleBaby   <$ babyClicked
                    , RoleParent <$ parentClicked
                    ]
