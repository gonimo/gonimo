{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Gonimo.Client.Family.UI where

import           Control.Lens
import           Data.Map                         (Map)
import           Data.Maybe                       (fromMaybe)
import           Data.Monoid
import           Control.Applicative
import           Data.Text                        (Text)
import qualified Gonimo.Client.Invite             as Invite
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

    -- elAttr "img" ("class" =: "welcome-img" <> "src" =: "/pix/world.png") $ blank
    elClass "div" "welcome-container" $
      elClass "div" "start-welcome-img" $ blank
    el "br" blank

    el "h3" $ text "Create a new Family"
    elClass "div" "welcome-form" $ do
      inputFieldClicked <-
        makeClickable
        $ elAttr' "input" ( "class" =: "welcome-input" <> "readonly" =: "true" <> "type" =: "text"
                            <> "placeholder" =: "Press >+<, I know you want to!"
                          ) blank

      plusClicked <-
        makeClickable
        $ elAttr' "div" ( "class" =: "input-btn plus" <> "title" =: "Create a family to get started."
                          <> "type" =: "button" <> "role" =: "button"
                        ) blank
      let userWantsFamily = leftmost [ plusClicked, inputFieldClicked ]
      pure $ UI never userWantsFamily never never never never

ui :: forall m t. (HasWebView m, MonadWidget t m) => App.Config t -> App.Loaded t -> Bool -> m (UI t)
ui appConfig loaded familyGotCreated = do
  (newFamilyResult, newFamilyReqs) <-
    createFamily appConfig loaded familyGotCreated
  elClass "div" "container" $ mdo
    el "script" $ text "screenfull.exit();"
    el "h1" $ do
      text "Welcome to the "
      el "wbr" blank
      text "Gonimo World!"
    el "br" blank

    elAttr "img" ("class" =: "welcome-img" <> "src" =: "/pix/world.svg") $ blank
    el "br" blank

    el "h3" $ text "FAMILY"
    (familySelected, clickedAdd, clickedLeave, nameChanged) <-
      familyChooser $ DefiniteFamily  (loaded^.App.families) (loaded^.App.selectedFamily)

    el "br" blank

    selectedView <- holdDyn "" $ leftmost [ const "isInviteView " <$> inviteRequested
                                          , const "" <$> invite^.Invite.uiGoBack
                                          , const "" <$> invite^.Invite.uiDone
                                          ]
    invite <-
      elDynClass "div" (pure "container fullScreenOverlay inviteView " <> selectedView) $ do
        firstCreation <- headE inviteRequested
        let inviteUI
              = Invite.ui loaded
                $ Invite.Config { Invite._configResponse = appConfig^.App.server.webSocket_recv
                                , Invite._configSelectedFamily = loaded^.App.selectedFamily
                                , Invite._configAuthenticated = appConfig^.App.auth.Auth.authenticated
                                , Invite._configCreateInvitation = never
                                }
        dynInvite <- widgetHold (pure def) $ const inviteUI <$> firstCreation
        pure $ Invite.inviteSwitchPromptlyDyn dynInvite

    (roleSelected, inviteRequested) <- roleSelector


    pure $ UI { _uiSelectFamily = familySelected
              , _uiCreateFamily = clickedAdd
              , _uiLeaveFamily = leftmost [ clickedLeave
                                          , push (\r -> pure $ r^?_CreateFamilyCancel) newFamilyResult
                                          ]
              , _uiSetName  = leftmost [ nameChanged
                                       , push (\r -> pure $ r^?_CreateFamilySetName) newFamilyResult
                                       ]
              , _uiRoleSelected = roleSelected
              , _uiRequest = newFamilyReqs <> invite^.Invite.request
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
                 => DefiniteFamily t -> m (Event t FamilyId, Event t (), Event t (), Event t Text)
familyChooser family' = mdo
  (clicked, clickedAdd, clickedLeave, nameChanged) <- do
    let cFamilyName = currentFamilyName family'
    elClass "div" "welcome-form" $ do
      clicked' <-
        makeClickable . elAttr' "div" (addBtnAttrs "family-select") $ do
          elClass "i" "fa fa-fw fa-users" blank
          text " "
          dynText $ zipDynWith getFamilyName (family'^.definiteSelected) (family'^.definiteFamilies)
          text " "
          elClass "span" "caret" blank

      (clickedAdd', clickedLeave') <-
        elClass "div" "input-btn-grp" $ do
          let mkMyButton t = makeClickable $ elAttr' "div" (addBtnAttrs t) blank
          let mkLeaveBtn = confirmationEl (makeClickable $ elAttr' "div" (addBtnAttrs "minus") blank)
                             (dynText $ pure "Really leave family '" <> cFamilyName <> pure "'?")
          (,) <$> mkMyButton "plus" <*> mkLeaveBtn

      nameChanged' <-
        editStringEl (makeClickable $ elAttr' "div" (addBtnAttrs "input-btn edit") blank)
        (text "Change your family name to ...")
        cFamilyName
      pure (clicked', clickedAdd', clickedLeave', nameChanged')

  let openClose = pushAlways (\_ -> not <$> sample (current droppedDown)) clicked
  droppedDown <- holdDyn False $ leftmost [ openClose
                                          , const False <$> selectedId
                                          ]
  let
    droppedDownClass :: Dynamic t Text
    droppedDownClass = fmap (\opened -> if opened then "isDroppedDown " else "") droppedDown
  let
    dropDownClass :: Dynamic t Text
    dropDownClass = pure "family-select dropDown " <> droppedDownClass

  selectedId :: Event t FamilyId <- elDynClass "div" dropDownClass $
    renderFamilySelectors family'
  pure (selectedId, clickedAdd, clickedLeave, nameChanged)
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


createFamily :: forall m t. (HasWebView m, MonadWidget t m) => App.Config t -> App.Loaded t -> Bool
  -> m (Event t CreateFamilyResult, Event t [API.ServerRequest])
createFamily appConfig loaded familyGotCreated = mdo
  let response = appConfig^.App.server.webSocket_recv
  let
    familyCreated :: Event t FamilyId
    familyCreated = push (\res ->
                              case res of
                                API.ResCreatedFamily fid -> pure $ Just fid
                                _ -> pure Nothing
                           ) response
  mFamilyId' :: Dynamic t (Maybe FamilyId) <- holdDyn Nothing $ leftmost [ Just <$> familyCreated
                                                                         , const Nothing <$> gotValidFamilyId'
                                                                         ]

  let gotValidFamilyId' = leftmost [ push (\fid -> do
                                           cFamilies <- sample . current $ loaded^.App.families
                                           pure $ cFamilies ^. at fid
                                          ) familyCreated
                                   , push (\cFamilies -> do
                                           mFid <- sample $ current mFamilyId'
                                           pure $ do
                                             fid <- mFid
                                             cFamilies ^. at fid
                                          ) (updated (loaded^.App.families))
                                     ]
  let uiTrue = elClass "div" "fullScreenOverlay" $ createFamily' appConfig loaded
  let uiFalse = pure (never, never)

  let startUI = if familyGotCreated then uiTrue else uiFalse

  let uiEv = leftmost [ const uiTrue <$> gotValidFamilyId'
                      , const uiFalse
                        <$> push (\ev -> pure
                                        $ ev^?_CreateFamilyCancel
                                        <|> ev^?_CreateFamilyOk
                                ) createFamilyEv
                      ]

  evEv <- widgetHold startUI uiEv

  let createFamilyEv = switchPromptlyDyn (fst <$> evEv)
  let reqs = switchPromptlyDyn  (snd <$> evEv)
  pure (createFamilyEv, reqs)

-- Dialog to configure family when a new one get's created:
createFamily' :: forall m t. (HasWebView m, MonadWidget t m) => App.Config t -> App.Loaded t
  -> m (Event t CreateFamilyResult, Event t [API.ServerRequest])
createFamily' appConfig loaded = mdo
  let showNameEdit = const "isFamilyNameEdit" <$> invite^.Invite.uiGoBack
  let showInviteView = const "isInviteView" <$> familyEditOk

  selectedView <- holdDyn "isFamilyNameEdit" $ leftmost [showNameEdit, showInviteView]

  (familyEditBack, familyEditOk) <-
    elDynClass "div" (pure "container familyNameEdit " <> selectedView) $ do
      familyEditName loaded (invite^.Invite.uiGoBack)

  invite <-
    elDynClass "div" (pure "container inviteView " <> selectedView) $ do
      firstCreation <- headE showInviteView
      let inviteUI
            = Invite.ui loaded
            $ Invite.Config { Invite._configResponse = appConfig^.App.server.webSocket_recv
                            , Invite._configSelectedFamily = loaded^.App.selectedFamily
                            , Invite._configAuthenticated = appConfig^.App.auth.Auth.authenticated
                            , Invite._configCreateInvitation = never
                            }
      dynInvite <- widgetHold (pure def) $ const inviteUI <$> firstCreation
      pure $ Invite.inviteSwitchPromptlyDyn dynInvite

  let doneEv = leftmost [ const CreateFamilyOk <$> invite^.Invite.uiDone
                        , const CreateFamilyCancel <$> familyEditBack
                        , CreateFamilySetName <$> familyEditOk
                        ]

  pure (doneEv, invite^.Invite.request)

familyEditName :: forall m t. (HasWebView m, MonadWidget t m)
                  => App.Loaded t -> Event t () -> m (Event t (), Event t Text)
familyEditName loaded reactivated' = do
    reactivated <- delay 0.2 reactivated' -- necessary because focus isn't triggered otherwise
    backClicked <- makeClickable . elAttr' "div" (addBtnAttrs "back-arrow") $ blank
    el "br" blank
    el "br" blank

    el "h1" $ text "Create New Family"
    elClass "div" "welcome-form" $ do
      genName <- sample . current $ App.currentFamilyName loaded
      nameInput <- textInput $ (def :: TextInputConfig t)
        { _textInputConfig_attributes = (pure $ "class" =: "welcome-input")
        , _textInputConfig_inputType = "text"
        , _textInputConfig_initialValue = genName
        , _textInputConfig_setValue = updated (App.currentFamilyName loaded)
        }
      -- Handle focus:
      addFocusPostBuild $ nameInput^.textInput_builderElement
      performEvent_ $ const (addFocus $ nameInput^.textInput_builderElement)
                      <$> leftmost [ const () <$> updated (App.currentFamilyName loaded)
                                   , reactivated
                                   ]

      okClicked <- makeClickable . elAttr' "div" (addBtnAttrs "input-btn check ") $ blank
      let nameValue = current $ nameInput^.textInput_value
      let confirmed = leftmost [ okClicked, keypress Enter nameInput ]
      pure (backClicked, tag nameValue confirmed)
