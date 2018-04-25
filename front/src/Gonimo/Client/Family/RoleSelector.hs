{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Gonimo.Client.Family.RoleSelector where

import           Reflex.Dom.Core


import           Gonimo.Client.Reflex.Dom
import           Gonimo.Client.Prelude
import           Gonimo.Client.Family.RoleSelector.I18N
import           Gonimo.Client.Router (Route(..))




roleSelector :: forall model t m. GonimoM model t m
          => m (Event t Route)
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
    pure $ leftmost [ const RouteBaby <$> babyClicked
                    , const RouteParent <$> parentClicked
                    ]
