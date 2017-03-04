{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Gonimo.Client.Family.RoleSelector where

import           Reflex.Dom.Core

import           Gonimo.Client.Family.Internal
import           Gonimo.Client.Reflex.Dom
import           Gonimo.Client.Prelude



roleSelector :: forall t m. (MonadFix m, DomBuilder t m, MonadHold t m, PostBuild t m)
          => m (Event t GonimoRole)
roleSelector = do
  let mkRoleButton inner = do
        elAttr "div" ("class" =: "btn-group" <> "role" =: "group") $
          buttonAttr ("class" =: "btn btn-default" <> "type" =: "button") inner
  elClass "div" "container hCenteredBox" $ do
    elClass "div" "btn-group btn-group-lg" $ do
      babyClicked <- mkRoleButton $ do
        text "Baby"
        elClass "span" "hidden-xs" $ text " Station"
      parentClicked <- mkRoleButton $ do
        text "Parent"
        elClass "span" "hidden-xs" $ text " Station"
      pure $ leftmost [ const RoleBaby <$> babyClicked
                      , const RoleParent <$> parentClicked
                      ]
