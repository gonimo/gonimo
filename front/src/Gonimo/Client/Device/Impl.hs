{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Gonimo.Client.Device.Impl
Description : Entry point to data from the server.
Copyright   : (c) Robert Klotzner, 2018
A device can create families, retrieve family data, can access it's account, ....
-}
module Gonimo.Client.Device.Impl ( -- * Interface
                               module API
                               -- * Types
                             , ModelConfig(..)
                             , HasModel
                             , HasModelConfig
                               -- * Creation
                             , make
                             ) where











import           Control.Arrow             (second)
import           GHCJS.DOM.Types           (MonadJSM)

import           Control.Monad.Fix         (mfix)
import qualified Data.Map                  as Map
import           Data.Set                  ((\\))
import qualified Data.Set                  as Set
import           Gonimo.Client.Server      (onResponse)
import           Reflex.Network
import           Reflex.NotReady.Class



import           Gonimo.Client.Device
import           Gonimo.Client.Family      (Family)
import qualified Gonimo.Client.Family.Impl as Family
import qualified Gonimo.Client.Host        as Host
import           Gonimo.Client.Model
import           Gonimo.Client.Prelude

import qualified Gonimo.Client.Account     as Account
import qualified Gonimo.Client.Auth        as Auth
import qualified Gonimo.Client.Server      as Server
import qualified Gonimo.Client.Subscriber  as Subscriber
import qualified Gonimo.SocketAPI          as API
import           Gonimo.SocketAPI.Types    (InvitationInfo, InvitationSecret)



-- | Our dependencies
type HasModel model = (Server.HasServer model, Host.HasHost model, Auth.HasAuth model
                      , Family.HasModel model, Account.HasAccount model
                      )

-- | Example datatype fulfilling 'HasModelConfig'.
data ModelConfig t
  = ModelConfig { -- | For subscribing important commands.
                  --
                  --   We subscribe important commands to ensure they will be
                  --   delivered even on connection problems.
                  _subscriberConfig :: Subscriber.Config t

                  -- | Commands going to the server.
                , _serverConfig     :: Server.Config t
                }

-- | Configurations we provide for the model as inputs.
type HasModelConfig c t = (IsConfig c t, Subscriber.HasConfig c, Server.HasConfig c)


-- | Create a Device.
--
make
  :: forall t m model mConf c
     . ( Reflex t, MonadHold t m, MonadFix m, MonadJSM m, NotReady t m, Adjustable t m, PostBuild t m
       , PerformEvent t m, TriggerEvent t m, MonadIO (Performable m)
       , HasModel model, HasConfig c, HasModelConfig mConf t
       )
     => model t -> c t -> m (mConf t, Device t)
make model conf = mfix $ \ ~(_, ourDevice) -> do
  let
    _identifier = fmap API.deviceId <$> model ^. Auth.authData
  (mConf, _selectedFamily) <- makeFamily model conf ourDevice
  pure (mConf, Device {..})

makeFamily
  :: forall t m model mConf c
     . ( Reflex t, MonadHold t m, MonadFix m, MonadJSM m, NotReady t m, Adjustable t m, PostBuild t m
       , PerformEvent t m, TriggerEvent t m, MonadIO (Performable m)
       , HasModel model, HasConfig c, HasModelConfig mConf t
       )
     => model t -> c t -> Device t -> m (mConf t, MDynamic t (Family t))
makeFamily model conf ourDevice = do
    let
      onSelectedFamily :: Event t API.FamilyId
      onSelectedFamily = fmapMaybe (^? API._ResSwitchedFamily . _2) $ model ^. onResponse

      onRequestInvitationIds = API.ReqGetFamilyInvitations <$> onSelectedFamily

      makeJustFamily :: API.FamilyId -> m (mConf t, Maybe (Family t))
      makeJustFamily = fmap (second Just) . Family.make model (conf ^. familyConfig)

      onFamily =  makeJustFamily <$> onSelectedFamily

    -- TODO: Missing: We have to clear the selection when the family list gets
    -- empty and we have to update it if our current id gets deleted and we have to initialize it from storage:
    initEv <- networkView =<< holdDyn (pure (mempty, Nothing)) onFamily

    famMConf <- flatten $ fst <$> initEv
    subscriberConf <- Subscriber.fromServerRequests $ (:[]) <$> onRequestInvitationIds
    dynFamily <- holdDyn Nothing $ snd <$> initEv

    let selectFamConf = selectFamily model conf ourDevice

    pure ( mconcat [ famMConf
                   ,subscriberConf
                   , selectFamConf
                   ]
         , dynFamily
         )

-- | Select a family based on user selection and available families.
--
--   We switch family once our current family gets deleted, we also switch
--   families if a new family becomes available.
selectFamily
  :: forall t model mConf c
     . ( Reflex t , HasModel model, HasConfig c, HasModelConfig mConf t)
     => model t -> c t -> Device t -> mConf t
selectFamily model conf ourDevice = mempty & Server.onRequest .~ onReq
  where
    onReq = fmap (:[]) . fmapMaybe id
            . attachWith (<*>) (fmap API.ReqSwitchFamily <$> currentIdentifier)
            $ pure <$> onSelect

    onSelect :: Event t API.FamilyId
    onSelect = leftmost [ conf ^. onSelectFamily
                        , onNeededSwitch
                        ]

    currentIdentifier = current $ ourDevice ^. identifier

    ourFamily = ourDevice ^. selectedFamily
    -- If we take into account that `_identifier` might change once set, we
    -- should not extrace [API.FamilyId] from families, but from the actual
    -- server response.
    ourFamIds :: MDynamic t [API.FamilyId]
    ourFamIds = fmap Map.keys <$> model ^. Account.families

    famIdentifier :: MDynamic t API.FamilyId
    famIdentifier = fmap Family._identifier <$> ourFamily

    -- A family switch is necessary, if the currently selected one ceases to
    -- exist or a new family was created. In the latter case, we automatically
    -- switch to this newly created/joined family.
    onNeededSwitch = push (\mNewFamilies -> do
                              mOldFamilies <- sample $ current ourFamIds
                              mcSelected <- sample $ current famIdentifier
                              case (mOldFamilies, mNewFamilies) of
                                (_, Nothing) -> pure Nothing -- No data, nothing to fix
                                (Nothing, Just newFams) -> pure $ fixInvalidKey mcSelected newFams
                                (Just oldFams, Just newFams) -> do
                                  let oldKeys = Set.fromList oldFams
                                  let newKeys = Set.fromList newFams
                                  let createdKey = headMay . Set.toList $ newKeys \\ oldKeys
                                  let fixedIfInvalid = fixInvalidKey mcSelected newFams
                                  pure $ createdKey <|> fixedIfInvalid
                          ) (updated ourFamIds)

    -- Fix keys that are invalid.
    -- A key is invalid if don't yet have any or if it does no longer exist.
    fixInvalidKey :: Maybe API.FamilyId -> [API.FamilyId] -> Maybe API.FamilyId
    fixInvalidKey Nothing valid = headMay valid
    fixInvalidKey (Just key) valid = if key `elem` valid
                                       then Nothing
                                       else headMay valid
