{-|
Module      : Gonimo.Client.Account
Description : User facing Account API.
Copyright   : (c) Robert Klotzner, 2018

Manipulate claimed invitations, create famililies, join families, ...
-}
module Gonimo.Client.Account where

import           Data.Map               (Map)

import           Gonimo.Client.Prelude
import           Gonimo.Client.Reflex   (DynamicMap)
import qualified Gonimo.SocketAPI       as API
import           Gonimo.SocketAPI.Types (InvitationInfo, InvitationReply,
                                         InvitationSecret)



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

             -- | Request a new `API.Family` for this account.
           , _onCreateFamily :: Event t ()
           } deriving (Generic)

-- | Account data.
--   All data belonging to the current active account should go here. Like
--   claimed invitations or user name, ...
data Account t
  = Account { -- | The `API.AccountId` of this account
              _identifier         :: MDynamic t API.AccountId
              -- | Invitations currently claimed by the account. (At the moment,
              --   just the ones claimed in this session.)
            , _claimedInvitations :: Dynamic t ClaimedInvitations

              -- | The families this account is a member of.
              --
              --   The Dynamic holds `Nothing` in case the families have not yet
              --   been fully loaded from the server.
            , _families           :: MDynamic t (Families t)
            }

-- | Family `Map`.
type Families t = DynamicMap t API.FamilyId API.Family

-- | Map type for claimed invitations.
--
--   Eventually (when we have ReqGetClaimedInvitations) this should become Map
--   InvitationId InvitationInfo
type ClaimedInvitations = Map InvitationSecret InvitationInfo

instance Reflex t => Default (Config t) where
  def = Config never never never

instance Reflex t => Semigroup (Config t) where
  (<>) = mappenddefault

instance Reflex t => Monoid (Config t) where
  mempty = def
  mappend = (<>)

instance Flattenable Config where
  flattenWith doSwitch ev
    = Config
      <$> doSwitch never (_onClaimInvitation <$> ev)
      <*> doSwitch never (_onAnswerInvitation <$> ev)
      <*> doSwitch never (_onCreateFamily <$> ev)

-- Auto generated lenses:



class HasConfig a42 where
  config :: Lens' (a42 t) (Config t)

  onClaimInvitation :: Lens' (a42 t) (Event t [InvitationSecret])
  onClaimInvitation = config . go
    where
      go :: Lens' (Config t) (Event t [InvitationSecret])
      go f config' = (\onClaimInvitation' -> config' { _onClaimInvitation = onClaimInvitation' }) <$> f (_onClaimInvitation config')


  onAnswerInvitation :: Lens' (a42 t) (Event t [ (InvitationSecret, InvitationReply) ])
  onAnswerInvitation = config . go
    where
      go :: Lens' (Config t) (Event t [ (InvitationSecret, InvitationReply) ])
      go f config' = (\onAnswerInvitation' -> config' { _onAnswerInvitation = onAnswerInvitation' }) <$> f (_onAnswerInvitation config')


  onCreateFamily :: Lens' (a42 t) (Event t ())
  onCreateFamily = config . go
    where
      go :: Lens' (Config t) (Event t ())
      go f config' = (\onCreateFamily' -> config' { _onCreateFamily = onCreateFamily' }) <$> f (_onCreateFamily config')


instance HasConfig Config where
  config = id


class HasAccount a42 where
  account :: Lens' (a42 t) (Account t)

  identifier :: Lens' (a42 t) (MDynamic t API.AccountId)
  identifier = account . go
    where
      go :: Lens' (Account t) (MDynamic t API.AccountId)
      go f account' = (\identifier' -> account' { _identifier = identifier' }) <$> f (_identifier account')


  claimedInvitations :: Lens' (a42 t) (Dynamic t ClaimedInvitations)
  claimedInvitations = account . go
    where
      go :: Lens' (Account t) (Dynamic t ClaimedInvitations)
      go f account' = (\claimedInvitations' -> account' { _claimedInvitations = claimedInvitations' }) <$> f (_claimedInvitations account')


  families :: Lens' (a42 t) (MDynamic t (Families t))
  families = account . go
    where
      go :: Lens' (Account t) (MDynamic t (Families t))
      go f account' = (\families' -> account' { _families = families' }) <$> f (_families account')


instance HasAccount Account where
  account = id

