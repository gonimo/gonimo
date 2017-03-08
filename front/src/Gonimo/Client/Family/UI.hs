{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Gonimo.Client.Family.UI where

import           Control.Lens
import           Data.Map                         (Map)
import           Data.Maybe                       (fromMaybe)
import           Data.Monoid
import           Data.Text                        (Text)
import qualified Gonimo.Client.DeviceList         as DeviceList
import qualified Gonimo.Client.Invite             as Invite
import           Gonimo.Client.Reflex
import           Gonimo.Db.Entities               (FamilyId)
import qualified Gonimo.Db.Entities               as Db
import qualified Gonimo.SocketAPI                 as API
import qualified Gonimo.Types                     as Gonimo
import           Reflex.Dom.Core

import qualified Gonimo.Client.App.Types          as App
import qualified Gonimo.Client.Auth               as Auth
import           Gonimo.Client.ConfirmationButton (confirmationEl)
import           Gonimo.Client.EditStringButton   (editStringEl)
import           Gonimo.Client.Family.Internal
import           Gonimo.Client.Family.RoleSelector
import           Gonimo.Client.Reflex.Dom
import           Gonimo.Client.Server             (webSocket_recv)


uiStart :: forall m t. (HasWebView m, MonadWidget t m) => m (UI t)
uiStart = do
  elClass "div" "container" $ do
    el "h1" $ do
      text "Welcome to the "
      el "wbr" blank
      text "Gonimo World!"
    el "br" blank

    elAttr "img" ("class" =: "welcome-img" <> "src" =: "/pix/world.png") $ blank
    el "br" blank

    el "h3" $ text "Create a new Family"
    elClass "div" "welcome-form" $ do
      elAttr "input" ( "class" =: "welcome-input" <> "readonly" =: "true" <> "type" =: "text"
                       <> "placeholder" =: "Press >+<, I know you want to!"
                     ) blank

      plusClicked <-
        makeClickable
        $ elAttr' "div" ( "class" =: "input-btn plus" <> "title" =: "Create a family to get started."
                          <> "type" =: "button" <> "role" =: "button"
                        ) blank
      pure $ UI never plusClicked never never never

ui :: forall m t. (HasWebView m, MonadWidget t m) => App.Loaded t -> m (UI t)
ui loaded =
  elClass "div" "container" $ do
    let cFamilyName = currentFamilyName
                      $ DefiniteFamily (loaded^.App.families) (loaded^.App.selectedFamily)
    el "h1" $ do
      text "Welcome to the "
      el "wbr" blank
      text "Gonimo World!"
    el "br" blank

    elAttr "img" ("class" =: "welcome-img" <> "src" =: "/pix/world.svg") $ blank
    el "br" blank

    el "h3" $ text "FAMILY"
    (familySelected, clickedAdd, clickedLeave, nameChanged) <-

    el "br" blank

    roleSelected <- roleSelector


    pure $ UI { _uiSelectFamily = familySelected
              , _uiCreateFamily = clickedAdd
              , _uiLeaveFamily = clickedLeave
              , _uiSetName  = nameChanged
              , _uiRoleSelected = roleSelected
              }


-- familyChooser :: forall m t. (HasWebView m, MonadWidget t m)
--                  => DefiniteFamily t -> m (Event t FamilyId)
-- familyChooser family' = do
--   initVal <- sample . current $ family'^.definiteSelected
--   let familyNames = fmap (Gonimo.familyName . Db.familyName) <$> family'^.definiteFamilies
--   dropdown' <-
--     dropdown initVal familyNames
--     $ DropdownConfig { _dropdownConfig_setValue = updated $ family'^.definiteSelected
--                      , _dropdownConfig_attributes = pure ("class" =: "family-select")
--                      }
--   pure $ dropdown'^.dropdown_change



familyChooser :: forall m t. (HasWebView m, MonadWidget t m)
                 => DefiniteFamily t -> m (Event t FamilyId)
familyChooser family' = mdo
  clicked <-
    makeClickable . elAttr' "div" (addBtnAttrs "family-select") $ do
      elClass "i" "fa fa-fw fa-users" blank
      text " "
      dynText $ zipDynWith getFamilyName (family'^.definiteSelected) (family'^.definiteFamilies)
      text " "
      elClass "span" "caret" blank

  let openClose = pushAlways (\_ -> not <$> sample (current droppedDown)) clicked
  droppedDown <- holdDyn False $ leftmost [ openClose
                                          , const False <$> selectedId
                                          ]
  let
    droppedDownClass :: Dynamic t Text
    droppedDownClass = fmap (\opened -> if opened then "isDroppedDown " else "") droppedDown
  let
    dropDownClass :: Dynamic t Text
    dropDownClass = pure "dropDown " <> droppedDownClass

  selectedId :: Event t FamilyId <- elDynClass "div" dropDownClass $
    renderFamilySelectors family'
  pure selectedId
  where
    getFamilyName :: FamilyId -> Map FamilyId Db.Family -> Text
    getFamilyName famId families'
      = fromMaybe "" $ families'^?at famId._Just.to (Gonimo.familyName . Db.familyName)

renderFamilySelectors :: forall m t. (HasWebView m, MonadWidget t m)
                    => DefiniteFamily t -> m (Event t FamilyId)
renderFamilySelectors family' = fmap fst <$> selectViewListWithKey (family'^.definiteSelected) (family'^.definiteFamilies) renderFamilySelector

-- Internal helper for familyChooser ...
renderFamilySelector :: forall m t. (HasWebView m, MonadWidget t m)
                    => FamilyId -> Dynamic t Db.Family -> Dynamic t Bool -> m (Event t ())
renderFamilySelector _ family' selected' = do
    el "div" $ do
      makeClickable . elAttr' "a" (addBtnAttrs "")
        $ dynText
          $ (Gonimo.familyName . Db.familyName <$> family') <> ffor selected' (\selected -> if selected then " ✔" else "")
