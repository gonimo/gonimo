{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Gonimo.SocketAPI.V1
Description : SocketAPI version 1.
Copyright   : (c) Robert Klotzner, 2017
API used for client - server communication, version 1.
-}

module Gonimo.SocketAPI.V1 where


import           Data.Aeson.Types             (FromJSON, ToJSON (..),
                                               defaultOptions,
                                               genericToEncoding, genericToJSON)
import           Data.Text                    (Text)
import           Data.Time.Clock              (UTCTime)
import           GHC.Generics                 (Generic)

import           Gonimo.Server.Error          (ServerError)
import           Gonimo.SocketAPI.Model
import qualified Gonimo.SocketAPI.View.Family as Family
import qualified Gonimo.SocketAPI.View.Account as Account


type FromId = DeviceId
type ToId = DeviceId

type ClientMessage = Text

-- | Messages from the client to the server.
--
--   Top level messages can be thought of being commands to the server to do
--   something, something more than just updating state. For example
--   'MakeDevice' makes the server create a secret, retrieve the current time
--   and so on and then create a new device with those properties.
--
--   In contrast 'APIEvent's in the 'ClientEvent' constructor are simple state
--   upating events containing all the needed information to perform the state
--   update. Therefore the server can also simply forward those messages to
--   other devices interested in the concerned state.
data FromClient
  -- | Client will send periodic ping messages to the server, for keeping the session alive.
  --
  --   We can't rely on WebSocket ping control messages, because the client
  --   can't see/handle them!
  --
  --   This is just a simple control message, no real state is updated. Although
  --   the device online status depends on regular pings.
  = Ping

  -- | Create a new device on the server.
  --
  --   This results in a machine account on the server for your current device.
  --   The server will respond with a `MadeDevice` message, with authentication
  --   data you can use for `Authenticate`.
  | MakeDevice !(Maybe Text)

  -- | Authenticate to the server.
  --
  --   All other messages except for `Ping` and `MakeDevice` won't get accepted
  --   without authenticating first. If you don't yet have the needed 'AuthToken',
  --   issue a `MakeDevice` command first for creating a machine account on the
  --   server.
  | Authenticate !AuthToken

  -- | Create a new family with your account being the only member.
  --
  --   Afterwards you can add further accounts to your family by inviting them
  --   via `MakeInvitation` and `SendInvitation`.
  | MakeFamily

  -- | Create a new invitation on the server.
  --
  --   This message will trigger the `Update` 'OnNewFamilyInvitation' which contains
  --   the secret you need to transmit to the device being invited.
  | MakeInvitation !FamilyId

  -- | The invited device can use this command for claiming the invitation for itself.
  --
  --   The server will respond with an `ClaimedInvitation` `APIEvent` which
  --   contains details about the invitation. After sending `ClaimInvitation`
  --   the invitation belongs to your device, other devices will no longer be allowed
  --   to claim or answer the claimed invitation.
  | ClaimInvitation !Secret

  -- | Answer an invitation.
  --
  --   You can either accept or decline the invitation. If you accept, then your
  --   device will become a member of the inviting family. If you decline, the
  --   invitation will simply be deleted from the server.
  | AnswerInvitation !InvitationId !InvitationReply

  -- | Send a message to another device in your currently selected family.
  --
  --   The server will simply forward the message to the other device. The only
  --   requirement is that both the sending and the receiving device are online
  --   in the same family.
  | SendMessage !ToId !ClientMessage

  -- | Send an update to the server.
  --
  --   Those updates will change the server state in some way and
  --   will be forwarded to other devices that might be concerned to update
  --   their local state accordingly.
  | UpdateServer !Update

  | Get !ViewSelector
  deriving (Generic, Show)

instance FromJSON FromClient
instance ToJSON FromClient where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

-- | Messages to the client from the server.
data ToClient
    -- | The server will respond with `Pong` on every `Ping`.
    --
    --   Those messages are used by both the client and the server to check
    --   whether the connection is still alive. The client will usually reconnect
    --   once it does not get a timely `Pong`. The server will to get rid of
    --   connections it did not receive a `Ping` for in a reasonable time frame.
  = Pong

    -- | If another client with the same 'DeviceId' comes online it will steal the session:
    --   After the server sent 'StoleSession', the client is no longer
    --   authenticated - the current session is invalid.
  | StoleSession

    -- | Response with  'AuthToken' for a newly created client, in response to 'MakeDevice'.
  | MadeDevice !AuthToken


    -- | Receive a message sent by another device.
  | ReceiveMessage !FromId !ClientMessage

    -- | Server updates for keeping the client's local state in sync.
  | UpdateClient !Update

    -- | Responses to `GetView` commands.
    --
    --   The server will respond with the current status of the specified views.
    --   The client should then keep its local copy in sync by handling
    --   `SeverEvent` messages.
  | Got !View

    -- | Some message could not be processed.
    --
    -- The server will include the offending `FromClient` message, so the client
    -- can map it back to issued requests. In addition `ServerError` are designed
    -- to be as self contained as possible.
  | ServerError !FromClient !ServerError
  deriving (Generic, Show)

instance ToJSON ToClient where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

-- | Idempotent update actions for updating state on both the client and the server.
--
--   Clients can send 'Update' messages by means of 'UpdateServer' to the server
--   for updating the server state, if the server accepts the update it will
--   perform it (updating the server state) and will forward the 'Update'
--   wrapped in an 'UpdateClient' message to all clients that hold a copy of the
--   affected data, including the client that sent the 'Update' message with
--   'UpdateServer'. The client knows that the server accepted the update if it
--   receives the same 'Update' message from the server in an 'UpdateClient'
--   message. In case the server does not accept an update, it will respond with
--   an appropriate 'ServerError'.
--
--   Clients can only update server state they previously got with 'Get' from the server.
--   Trying to update data not downloaded first via 'Get', might trigger
--   an 'DataNotCached' error from the server.

--   The server state can also be modified by clients by issuing appropriate
--   'FromClient' commands, for example by accepting an invitation with
--   'AnswerInvitation' or creating a new family with 'MakeFamily'. Those
--   commands will also trigger approriate 'UpdateClient' messages like:
--   'OnNewFamilyMember', 'OnRemovedFamilyInvitation' (it just got accepted) or
--   for 'MakeFamily': 'OnNewAccountFamily'.
--
--   All 'Update' messages might get sent by the server at some point, only some
--   might be sent clients.
data Update
  =
    --   Family updates

    -- | Change the family name.
    --
    --   Accepted from clients, that are members of the family and have any non 'Offline' 'DeviceStatus' in the family. See 'OnChangedDeviceStatus'.
    OnChangedFamilyName         !FamilyId !Text

    -- | When was the last time a device was online in this family.
    --
    --   Not accepted from clients, gets updated automatically by the server.
  | OnChangedFamilyLastAccessed !FamilyId !UTCTime

    -- | A new invitation for the family was created.
    --
    --   Not accepted from clients, but sent by the server if a client issued
    --   the 'MakeInvitation' command.
  | OnNewFamilyInvitation       !FamilyId !InvitationId

    -- | A family invitation got removed/accepted/declined.
    --
    --   An invitation got either removed by a client (by sending
    --   'OnRemovedFamilyInvitation') or accepted/declined.
    --
    --   Accepted from online family members.
  | OnRemovedFamilyInvitation !FamilyId !InvitationId

    --   Account updates
  | OnNewAccountDevice          !AccountId !DeviceId
  | OnRemovedAccountDevice      !AccountId !DeviceId

    -- | A new invitation got claimed by the account.
    --
    --   Not accepted from clients. Clients will receive this message when the
    --   issue a 'ClaimInvitation' command.
  | OnNewAccountInvitation      !AccountId !InvitationId

    -- | The account became member of another family.
    --
    --   Not accepted from clients, clients have to create new families with
    --   'MakeFamily' or accept invitations for becoming members of new
    --   families.
    --
    --   Gets send to the concerned account and all members of the family who
    --   are online.
  | OnNewFamilyAccount          !FamilyId !AccountId

    -- | A family member left the family.
    --
    --   Accepted from all not offline family members.
  | OnRemovedFamilyAccount      !FamilyId !AccountId

    --   Device updates

    -- | Change the name of a device.
    --
    --   Accepted from clients that are currently online in the same family as
    --   the targeted device.
  | OnChangedDeviceName         !DeviceId !Text

    -- | When was the device online the last time.
    --
    --   Automatically sent on changes by the server, not accepted by clients.
  | OnChangedDeviceLastAccessed !DeviceId !UTCTime

    -- | Clients have an online status ('DeviceStatus') for a given family.
    --
    --   Accepted from the client itself for updating the server. If the device
    --   was online in another family before, the members fo this family will
    --   receive an 'OnChangedDeviceStatus' with 'Offline' status, the now
    --   selected family will receive an 'OnChangedDeviceStatus' message of
    --   'Online'.
  | OnChangedDeviceStatus       !DeviceId !FamilyId !DeviceStatus

    --   Invitation updates

    -- | The invitation got claimed by some device.
    --
    --   Not accepted from clients, will get sent by the server to the inviting
    --   family members on 'ClaimInvitation' command.
  | OnClaimedInvitation         !InvitationId

    -- | The invitation got delivered by some means.
    --
    --   Accepted from clients, once other deliveries then email are supported.
    --   The message will be triggered by the server if a client issues a
    --   'SendInvitation' command.
  | OnChangedInvitationDelivery !InvitationId !InvitationDelivery
  deriving (Generic, Show)


instance FromJSON Update
instance ToJSON Update where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

-- | To be used with the 'Get' command for retrieving data from the server.
--
--   Data is organized in views, currently there are two views on data the
--   account view and the family view: The account view has views as seen by a
--   single account: E.g. what devices belong to the account, the families it is
--   a member of.
--
--  The family view is the view a family member has on data. E.g. other members,
--  open invitations, ...
--
--  There are no top-level selectors for account views and family views, the
--  server will send 'ViewAccount' and 'ViewFamily' messages upon authentication
--  and on 'OnChangedDeviceStatus' commands respectively.

--  The selectors can be used to fetch new
--  incremental data from the server, in case you received an 'Update' message
--  from the server. For example you can fetch a newly claimed invitation from
--  the server you just got informed about by a 'OnNewAccountInvitation' by
--  using 'Get' with 'SelectAccountInvitation'.
--
--  ** Receiving 'Update' messages
--  You will be receiving 'Update' messages regarding your account immediately
--  after being authenticated. You will also receive a 'ViewAccount' message.
--  You can keep this data in sync with the server by incrementaly applying the
--  sent 'Update' messages, retrieving additional data via 'Get' as indicated by
--  the 'Update' messages.
--
--  For a family you will be receiving 'Update' messages for the family you are
--  currently online in. See 'OnChangeDeviceStatus'. Also the server will only
--  accept 'Get' messages for the family you are currently online.
--
--  ** Sending 'Update' messages:
--  You can only reliably send 'Update' messages for data you already hold. Once
--  you hold a copy of some data you can expect the server to accept updates,
--  assuming you have the needed permissions of course.

data ViewSelector
  = SelectAccountDevice !DeviceId
  | SelectAccountFamily !FamilyId
  | SelectAccountInvitation !InvitationId

  | SelectFamilyAccount !AccountId
  | SelectFamilyDevice !DeviceId
  | SelectFamilyInvitation !InvitationId
  deriving (Eq, Ord, Generic, Show)


instance FromJSON ViewSelector
instance ToJSON ViewSelector where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions


-- | The data you selected with 'Get' and 'ViewSelector' will be transmitted to
--   you in a 'Got' message with the appropriate 'View' data.
data View
     -- | Will be sent to you upon successful authentication.
  = ViewAccount !AccountId !Account.View
  | ViewAccountDevice !DeviceId !Account.DeviceView
  | ViewAccountFamily !FamilyId !Account.FamilyView
  | ViewAccountInvitation !InvitationId !Account.InvitationView

  -- | Will be sent to you on 'OnChangedDeviceStatus' - if the 'FamilyId'
  --   differs from the previously selected one.
  | ViewFamily !FamilyId !Family.View
  | ViewFamilyAccount !AccountId !Family.AccountView
  | ViewFamilyDevice !AccountId !(DeviceId, Family.DeviceView)
  | ViewFamilyInvitation !InvitationId !Family.InvitationView
  deriving (Generic, Show)

instance ToJSON View where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

data InvitationReply = InvitationAccept | InvitationReject deriving (Generic, Show, Eq, Ord)

instance FromJSON InvitationReply
instance ToJSON InvitationReply where
  toEncoding = genericToEncoding defaultOptions
