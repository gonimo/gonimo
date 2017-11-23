{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Gonimo.SocketAPI.V1
Description : SocketAPI version 1.
Copyright   : (c) Robert Klotzner, 2017
API used for client - server communication, version 1.
-}

module Gonimo.SocketAPI.V1 where

import           Control.Lens
import           Data.Aeson.Types       (FromJSON, ToJSON (..), defaultOptions,
                                         genericToEncoding)
import           Data.Text              (Text)
import           GHC.Generics           (Generic)

import           Gonimo.Server.Error    (ServerError)
import           Gonimo.SocketAPI.Model
import qualified Gonimo.SocketAPI.View.Family as Family


type FromId = DeviceId
type ToId = DeviceId

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
  --   can't see/handle them! This is just a simple control message, no real
  --   state is updated. Although the device online status depends on regular
  --   pings.
  = Ping

  -- | Create a new device on the server.
  --
  --   This results in a machine account on the server for your current device.
  --   The server will respond with a `MadeDevice` message, with authentication
  --   data you can use for `Authenticate`.
  | MakeDevice !(Maybe Text)

  -- | Authenticate to the server.
  --
  --   All other messages except for `Ping` and `MakeDevice `won't get accepted
  --   without authenticating first. If you don't yet have the needed data,
  --   issue a `MakeDevice` command first for creating a machine account on the
  --   server.
  | Authenticate !AuthToken

  -- | Create a new family with your account being the only member.
  --
  --   Afterwards you can add further accounts to your family by inviting them via `MakeInvitation` and `SendInvitation`.
  | MakeFamily

  -- | Create a new invitation on the server.
  --
  --   This message will trigger the `APIEvent` `MadeInvitation` which contains
  --   the secret you need to transmit to the device being invited.
  | MakeInvitation !FamilyId

  -- | The invited device can use this command for claiming the invitation for itself.

  --   The server will respond with an `ClaimedInvitation` `APIEvent` which
  --   contains details about the invitation. After sending `ClaimInvitation`
  --   the invitation belongs to your device, other devices will no longer be allowed
  --   to claim or answer the claimed invitation.
  | ClaimInvitation !Secret

  -- | Answer an invitation.
  --
  --   You can either accept or decline the invitation. If you accept, then your
  --   device will become a member of the inviting family. If you decline, the
  --   invitation will simply be deleted from the server and nothing happens.
  | AnswerInvitation !Secret !InvitationReply

  -- | Send a message to another device in your currently selected family.
  --
  --   The server will simply forward the message to the other device. The only
  --   requirement is that both the sending and the receiving device are online
  --   in the same family. This means their accounts have belong to the same
  --   family and both devices issued a `SwitchFamily` `APIEvent` with the same `FamilyId`.
  | SendMessage !ToId !ClientMessage

  -- | Send an event to the server.
  --
  --   Those events will most likely change the server state in some way and
  --   will be forwarded to other devices that might be concerned to update
  --   their local state accordingly.
  | ClientEvent !APIEvent


-- | Messages to the client from the server.
data ToClient
  -- | The server will respond with `Pong` on every `Ping`.
  --
  --   Those messages are used by both the client and the server to check
  --   whether the connection is still alive. The client will usually reconnect
  --   once it does not get a timely `Pong`. The server will to get rid of
  --   connections it did not receive a `Ping` for in a reasonable time frame.
  = Pong -- ^ Response to `Ping`

  -- | If another client with the same device id comes online it will steal the session:
  --   After the server sent `StoleSession`, the client is no longer
  --   authenticated - the current session is invalid.
  | StoleSession

  -- | Response with auth data for a newly created client, in response to `MakeDevice`.
  | MadeDevice !Client.AuthData


  -- | Receive a message sent by another device.
  | ReceiveMessage !FromId !ClientMessage

  -- | Server events for keeping the client's local state in sync.
  | ServerEvent !APIEvent

  -- | Responses to `GetView` commands.
  --
  --   The server will respond with the current status of the specified views.
  --   The client should then keep its local copy in sync by handling
  --   `SeverEvent` messages.
  | View !View

  -- | Some message could not be processed.
  --
  -- The server will include the offending `FromClient` message, so the client
  -- can map it back to issued requests. In addition `ServerError` are designed
  -- to be as self contained as possible.
  | ServerError !FromClient !ServerError


-- | Idempotent events for updating state.
--
--   Clients can send `APIEvent`s to the server via `ClientEvent`, those events
--   will trigger updates in the server state and will be forwarded to concerned
--   clients via `ServerEvent`.
--
--   Clients are expected to retrieve data from the server via `GetView` once
--   they authenticated, afterwards there are expected to keeping their local state
--   in sync by handling `APIEvent` messages from the server appropriately.
--
--   The server will send `ServerEvent` messages immediately. You will get
--   messages that affect your account data and messages concerning your
--   currently selected family.
data APIEvent
  -- | A client changed it's `DeviceStatus`.
  --
  --   `Offline`: The device is considered offline chosen family.
  --
  --   `BabyStation`: The device is online and a baby station, accepting
  --   connections from other stations.
  --
  --   `ParentStation`: The device is online and might connect to baby stations.
  --
  --   A client can send `SetStatus` for selecting one of those modes and for
  --   selecting the family it will have this status in. In all other families
  --   it will have status `Offline`. This means, the server will forward the
  --   `SetStatus` message to clients in the selected family and will send a
  --   newly created `SetStatus` message to the family the device previously had
  --   a non `Offline` status (if any) with `DeviceStatus` being `Offline`.
  = SetStatus         !DeviceId !FamilyId !DeviceStatus

  -- | A device successfully created a new family.
  | MadeFamily        !AccountId !(FamilyId, Account.FamilyView)

  -- | A device name got changed.
  | SetDeviceName     !DeviceId !Text

  -- | Change the name of the family.
  --
  --   Currently you are only allowed to change the name of the family you have
  --   a `DeviceStatus` other than `Offline`.
  | SetFamilyName     !FamilyId !Text

  -- | An invitation was created.
  --
  --   Clients can update their family invitation view and forward the secret of
  --   the invitation to devices they want invited.
  | MadeInvitation    !FamilyId !(InvitationId, Family.InvitationView)

  -- | An invitation was sent via some means.
  --
  --   Clients can update their view on the invitation.
  | SendInvitation    !InvitationId !Client.SendInvitation

  -- | Get rid of an invitation:
  --
  --   This event can be triggered by clients in the family causing the server
  --  to delete the invitation. The server will forward the event to all other
  --  clients in the family, so they can update their state accordingly.
  --
  --  This event gets also sent by the server if an invited client declined or
  --  accepted the invitation.
  | DeleteInvitation  !InvitationId

  -- | A device claimed an invitation.
  --
  --   If a device claimed an invitation via 'ClaimInvitation' the server will
  --   respond with 'ClaimedInvitation' with a 'Just' 'Account.InvitationView'
  --   containing more details about the invitation.
  --
  --   Family members of the inviting family will receive this event too, but
  --   with 'Nothing'.
  | ClaimedInvitation !InvitationId !(Maybe Account.InvitationView)

  -- | A client accepted the invitation.
  --
  --   The message contains all relevant data about the new family member. The
  --   device will be initially offline.
  | AcceptInvitation  !(AccountId, Family.AccountView) !InvitationId

  -- | A device is no longer part of this family.
  --
  --   A device sending this message will get removed from the family (it will
  --   no longer be able to use it with `SetStatus`). The `LeaveFamily`
  --   message will be forwarded to the remaining members of the family.
  | LeaveFamily       !FamilyId !AccountId


data ViewSelector
  = SelectClaimedInvitations !AccountId
  | SelectAccountFamilies !AccountId
  | SelectAccountDevices !AccountId
  deriving (Eq, Ord)


data View
  = ViewClaimedInvitations !(Map Secret InvitationInfo)
  | ViewAccountFamilies !(Map FamilyId, Account.FamilyView)
  | ViewAccountDevices !(Map DeviceId, Account.DeviceView)
