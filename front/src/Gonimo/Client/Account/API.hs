{-|
Module      : Gonimo.Client.Account.API
Description : User facing Account API
Copyright   : (c) Robert Klotzner, 2018
-}
module Gonimo.Client.Account.API where

import           Data.Map               (Map)

import           Gonimo.Client.Prelude
import           Gonimo.SocketAPI.Types (InvitationInfo, InvitationReply)
import           Gonimo.Types           (InvitationSecret)



-- | Configuration for creating an account.
--
--   Currently this just handles accepting invitations.
data Config t
  = Config { -- | Claim an invitation by providing it's secret.
             --
             --   When the user clicks an invitation link it gets claimed,
             --   making the invitation unavailable for other parties.
             _onClaimInvitation :: Event t [InvitationSecret]

             -- | Answer an invitation. (Decline/accept it.)
           , _onAnswerInvitation :: Event t [(InvitationSecret, InvitationReply)]
           } deriving (Generic)

-- | Account data.
--   All data belonging to the current active account should go here. Like
--   claimed invitations or user name, ...
data Account t
  = Account { -- | Invitations currently claimed by the account. (At the moment,
              --   just the ones claimed in this session.)
              _claimedInvitations :: Dynamic t ClaimedInvitations
            }

-- | Map type for claimed invitations.
--
--   Eventually (when we have ReqGetClaimedInvitations) this should become Map
--   InvitationId InvitationInfo
type ClaimedInvitations = Map InvitationSecret InvitationInfo

instance Reflex t => Default (Config t) where
  def = Config never never

instance Reflex t => Semigroup (Config t) where
  (<>) = mappenddefault

instance Reflex t => Monoid (Config t) where
  mempty = memptydefault
  mappend = (<>)

instance Flattenable Config where
  flattenWith doSwitch ev
    = Config
      <$> doSwitch never (_onClaimInvitation <$> ev)
      <*> doSwitch never (_onAnswerInvitation <$> ev)

-- Auto generated lenses:


class HasConfig a where
  config :: Lens' (a t) (Config t)

  onClaimInvitation :: Lens' (a t) (Event t [InvitationSecret])
  onClaimInvitation = config . go
    where
      go :: Lens' (Config t) (Event t [InvitationSecret])
      go f config' = (\onClaimInvitation' -> config' { _onClaimInvitation = onClaimInvitation' }) <$> f (_onClaimInvitation config')


  onAnswerInvitation :: Lens' (a t) (Event t [ (InvitationSecret, InvitationReply) ])
  onAnswerInvitation = config . go
    where
      go :: Lens' (Config t) (Event t [ (InvitationSecret, InvitationReply) ])
      go f config' = (\onAnswerInvitation' -> config' { _onAnswerInvitation = onAnswerInvitation' }) <$> f (_onAnswerInvitation config')


instance HasConfig Config where
  config = id


class HasAccount a where
  account :: Lens' (a t) (Account t)

  claimedInvitations :: Lens' (a t) (Dynamic t ClaimedInvitations)
  claimedInvitations = account . go
    where
      go :: Lens' (Account t) (Dynamic t ClaimedInvitations)
      go f account' = (\claimedInvitations' -> account' { _claimedInvitations = claimedInvitations' }) <$> f (_claimedInvitations account')


instance HasAccount Account where
  account = id

