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
import           Gonimo.Client.ConfirmationButton (confirmationButton)
import           Gonimo.Client.EditStringButton   (editStringButton)
import           Gonimo.Client.Family.Internal
import           Gonimo.Client.Family.RoleSelector
import           Gonimo.Client.Reflex.Dom
import           Gonimo.Client.Server             (webSocket_recv)

-- Overrides configCreateFamily && configLeaveFamily
ui :: forall m t. (HasWebView m, MonadWidget t m)
            => App.Config t -> App.Loaded t -> m (UI t)
ui _ loaded = mdo
    let cFamilyName = currentFamilyName
                      $ DefiniteFamily (loaded^.App.families) (loaded^.App.selectedFamily)


    elClass "div" "container" $ do
      familySelected <- elClass "ul" "nav navbar-nav" $
                        familyChooser' $ DefiniteFamily  (loaded^.App.families) (loaded^.App.selectedFamily)
      clickedAdd <- buttonAttr ("class" =: "btn btn-default") $ text "+"
      clickedLeave <- confirmationButton ("class" =: "btn btn-danger") (text "-")
                        (dynText $ pure "Really leave family '" <> cFamilyName <> pure "'?")
      nameChanged <- editStringButton ("class" =: "btn btn-default")
                        ( elAttr "i" ( "class" =: "fa fa-fw fa-pencil"
                                    <> "data-toggle" =: "tooltip"
                                    <> "data-placement" =: "right"
                                    <> "title" =: "edit family name"
                                    ) blank
                        )
                        (text "Change your family name to ...")
                        cFamilyName

      roleSelected <- roleSelector
      -- devicesReqs <-  devices appConfig loaded deviceList


      pure $ UI { _uiSelectFamily = familySelected
                , _uiCreateFamily = clickedAdd
                , _uiLeaveFamily = clickedLeave
                , _uiSetName  = nameChanged
                , _uiRoleSelected = roleSelected
                }


-- Either create family button or family chooser depending on whether families exist or not.
familyChooser :: forall m t. (HasWebView m, MonadWidget t m)
                 => Family t -> m (Event t (Either () FamilyId))
familyChooser family' = do
  evFamilies <- waitForJust (family'^.families)

  let onFamilies families' =
        fromMaybeDyn
          (do
              clickedAdd <- buttonAttr ("class" =: "btn btn-default navbar-btn") $ text "+"
              pure $ Left <$> clickedAdd
          )
          (\selected -> do
              selEv <- familyChooser' (DefiniteFamily families' selected)
              pure $ Right <$> selEv
          )
          (family'^.selectedFamily)
  let noFamilies = do
        pure never
  dynEvEv <- widgetHold noFamilies (onFamilies <$> evFamilies)
  let evEv = switchPromptlyDyn dynEvEv -- Flatten Dynamic Event Event
  switchPromptly never evEv


familyChooser' :: forall m t. (HasWebView m, MonadWidget t m)
                 => DefiniteFamily t -> m (Event t FamilyId)
familyChooser' family' = do
  elAttr "li" ( "class" =: "dropdown" <> "data-toggle" =: "collapse" ) $ do
    elAttr "a" ( "class" =: "dropdown-toggle" <> "href" =: "#"
                <> "role" =: "button" <> "data-toggle" =: "dropdown"
                <> "type" =: "button") $ do
      elClass "span" ".h1" $ do
        elClass "i" "fa fa-fw fa-users" blank
        text " "
        dynText $ zipDynWith getFamilyName (family'^.definiteSelected) (family'^.definiteFamilies)
        text " "
        elClass "span" "caret" blank
    selectedId <- elClass "ul" "dropdown-menu" $ do
      elAttr "li" ("data-toggle" =: "collapse") $
        elAttr "div" ("class" =: "dropdown-header") $
        text "Switch to family:"
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
    elAttr "li" ("data-toggle" =: "collapse") $ do
      fmap (domEvent Click . fst) . elAttr' "a" ("type" =: "button" <> "role" =: "button")
        $ dynText
          $ (Gonimo.familyName . Db.familyName <$> family') <> ffor selected' (\selected -> if selected then " ✔" else "")

devices ::forall m t. (HasWebView m, MonadWidget t m)
            => App.Config t
            -> App.Loaded t
            -> DeviceList.DeviceList t
            -> m (Event t [API.ServerRequest])
devices appConfig loaded deviceList = do
    devListReqs <- DeviceList.ui loaded deviceList
    elClass "div" "" $ do
      elClass "div" "page-header" $
        elClass "h4" "" $ text "Invite More Devices to Join Your Family"
      elClass "div" "" $ do
        invite <- Invite.ui $ Invite.Config { Invite._configResponse = appConfig^.App.server.webSocket_recv
                                            , Invite._configSelectedFamily = loaded^.App.selectedFamily
                                            , Invite._configCreateInvitation = never
                                            , Invite._configAuthenticated = appConfig^.App.auth.Auth.authenticated
                                            }
        pure $ invite^.Invite.request <> devListReqs
