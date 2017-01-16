{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Gonimo.Client.Family.UI where

import Reflex.Dom
import Control.Monad
import Data.Monoid
import Data.Text (Text)
import Gonimo.Db.Entities (FamilyId)
import qualified Gonimo.Db.Entities as Db
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Gonimo.SocketAPI.Types as API
import qualified Gonimo.SocketAPI as API
import Control.Lens
import qualified GHCJS.DOM.JSFFI.Generated.Window as Window
import qualified Gonimo.Client.Storage as GStorage
import qualified Gonimo.Client.Storage.Keys as GStorage
import qualified GHCJS.DOM as DOM
import Data.Foldable (traverse_)
import Gonimo.Client.Reflex
import Data.Maybe (fromMaybe)
import qualified Gonimo.Types as Gonimo

import Gonimo.Client.Family.Internal

-- Overrides configCreateFamily
ui :: forall m t. (HasWebView m, MonadWidget t m)
            => Config t -> Family t -> m (Config t)
ui config family' = do
  config' <- familyChooser config family'
  clicked <- button "+"
  pure $ config' & configCreateFamily .~ clicked


familyChooser :: forall m t. (HasWebView m, MonadWidget t m)
                 => Config t -> Family t -> m (Config t)
familyChooser config family' = do
  elAttr "div" ( "class" =: "dropdown" <> "data-toggle" =: "collapse" ) $ do
    elAttr "button" ( "class" =: "dropdown-toggle btn btn-primary" <> "href" =: "#"
                <> "role" =: "button" <> "data-toggle" =: "dropdown"
                <> "type" =: "button") $ do
      elClass "i" "fa fa-fw fa-users" blank
      text " "
      dynText $ zipDynWith getFamilyName (family'^.selectedFamily) (family'^.families)
      text " "
      elClass "span" "caret" blank
    selectedId <- elClass "ul" "dropdown-menu" $ renderFamilySelectors family'
    pure $ config & configSelectFamily .~ selectedId
  where
    getFamilyName :: Maybe FamilyId -> Map FamilyId Db.Family -> Text
    getFamilyName (Just famId) families'
      = fromMaybe "" $ families'^?at famId._Just.to (Gonimo.familyName . Db.familyName)
    getFamilyName Nothing _ = ""

renderFamilySelectors :: forall m t. (HasWebView m, MonadWidget t m)
                    => Family t -> m (Event t FamilyId)
renderFamilySelectors family' = do
  readyEv <- waitForJust (family'^.selectedFamily)

  let
    noFamilies :: m (Event t FamilyId)
    noFamilies = elAttr "li" ("data-toggle" =: "collapse") (text "No family") *> pure never

  fmap switchPromptlyDyn
    . widgetHold noFamilies
    . ffor readyEv $ \selected ->
                       fmap fst <$> selectViewListWithKey selected (family'^.families) renderFamilySelector

-- Internal helper for familyChooser ...
renderFamilySelector :: forall m t. (HasWebView m, MonadWidget t m)
                    => FamilyId -> Dynamic t Db.Family -> Dynamic t Bool -> m (Event t ())
renderFamilySelector famId family' selected' = do
    elAttr "li" ("data-toggle" =: "collapse") $ do
      fmap (domEvent Click . fst) . el' "a"
        $ dynText
          $ (Gonimo.familyName . Db.familyName <$> family') <> ffor selected' (\selected -> if selected then " ✔" else "")

