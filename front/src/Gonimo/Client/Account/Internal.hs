{-|
Module      : Gonimo.Client.Account.Internal
Description : Types and internal functions for "Gonimo.Client.Account"
Copyright   : (c) Robert Klotzner, 2018
-}
module Gonimo.Client.Account.Internal where

import           Data.Map (Map)
import qualified Data.Map as Map

import           Gonimo.Client.Prelude
import           Gonimo.Client.Server (Server)
import qualified Gonimo.Client.Server as Server
import           Gonimo.SocketAPI
import           Gonimo.Types (InvitationSecret)
import           Gonimo.SocketAPI.Types (InvitationInfo, InvitationReply)



-- | Configuration for creating an account.
--
--   Currently this just handles accepting invitations.
data Config t
  = Config { -- | Claim an invitation by providing it's secret.
             _onClaimInvitation :: Event t InvitationSecret
             -- | Answer an invitation. (Decline/accept it.)
           , _onAnswerInvitation :: Event t (InvitationSecret, InvitationReply)

           , __server :: Server t
           }

-- | Account data.
--   All data belonging to the current active account should go here. Like
--   claimed invitations or user name, ...
data Account t
  = Account { -- | Invitations currently claimed by the account. (At the moment,
              --   just the ones claimed in this session.)
              _claimedInvitations :: Dynamic t ClaimedInvitations

              -- | Commands going to the server.
            , _serverConfig :: Server.Config t
            }

-- | Map type for claimed invitations.
--
--   Eventually (when we have ReqGetClaimedInvitations) this should become Map
--   InvitationId InvitationInfo
type ClaimedInvitations = Map InvitationSecret InvitationInfo


makeClaimedInvitations
  :: forall t m. (Reflex t, MonadHold t m, MonadFix m)
  => Config t -> m (Dynamic t ClaimedInvitations)
makeClaimedInvitations conf =
  let
    onClaimedInvitation :: Event t (InvitationSecret, InvitationInfo)
    onClaimedInvitation = fmapMaybe (^?_ResClaimedInvitation) $ conf ^. Server.response

    onAnsweredInvitation :: Event t InvitationSecret
    onAnsweredInvitation = fmapMaybe (^?_ResAnsweredInvitation._1) $ conf ^. Server.response
  in
    foldDyn id Map.empty $ leftmost [ uncurry Map.insert <$> onClaimedInvitation
                                    , Map.delete <$> onAnsweredInvitation
                                    ]

makeServerConfig :: Reflex t => Config t -> Server.Config t
makeServerConfig conf =
  let
    onClaim = (:[]) . ReqClaimInvitation <$> conf^.onClaimInvitation
    onAnswer = (:[]) . uncurry ReqAnswerInvitation <$> conf^.onAnswerInvitation

  in
    def & Server.request .~ mconcat [ onClaim
                                    , onAnswer
                                    ]

instance Server.HasConfig Account where
  config = serverConfig

instance Server.HasServer Config where
  server = _server

-- Auto generated lenses:


class HasConfig a where
  config :: Lens' (a t) (Config t)

  onClaimInvitation :: Lens' (a t) (Event t InvitationSecret)
  onClaimInvitation = config . go
    where
      go :: Lens' (Config t) (Event t InvitationSecret)
      go f config' = (\onClaimInvitation' -> config' { _onClaimInvitation = onClaimInvitation' }) <$> f (_onClaimInvitation config')


  onAnswerInvitation :: Lens' (a t) (Event t (InvitationSecret, InvitationReply))
  onAnswerInvitation = config . go
    where
      go :: Lens' (Config t) (Event t (InvitationSecret, InvitationReply))
      go f config' = (\onAnswerInvitation' -> config' { _onAnswerInvitation = onAnswerInvitation' }) <$> f (_onAnswerInvitation config')


  _server :: Lens' (a t) (Server t)
  _server = config . go
    where
      go :: Lens' (Config t) (Server t)
      go f config' = (\_server' -> config' { __server = _server' }) <$> f (__server config')


instance HasConfig Config where
  config = id

class HasAccount a where
  account :: Lens' (a t) (Account t)

  claimedInvitations :: Lens' (a t) (Dynamic t ClaimedInvitations)
  claimedInvitations = account . go
    where
      go :: Lens' (Account t) (Dynamic t ClaimedInvitations)
      go f account' = (\claimedInvitations' -> account' { _claimedInvitations = claimedInvitations' }) <$> f (_claimedInvitations account')


  serverConfig :: Lens' (a t) (Server.Config t)
  serverConfig = account . go
    where
      go :: Lens' (Account t) (Server.Config t)
      go f account' = (\serverConfig' -> account' { _serverConfig = serverConfig' }) <$> f (_serverConfig account')


instance HasAccount Account where
  account = id

