# Invitation System

Gonimo uses the concept of a family metaphor. Only devices within the same
family can /see/ each other. For joining a family an invitation system is used.

Devices in a family can send invitations to other devices, those devices can
accept the invitation in order to become a part of the family.

## Secure Implementation

The to be invited device receives a link, for example by email, containing a
secret which points to an invitation object on the server. This secret has to be
valid only a single time in order to be secure.

On the other hand the user also should be able to view the invitation before
accepting or rejecting it.

Those two requirements result in the following invitation accepting process:

1. The user does a put on "invitationInfo/<invitationSecret>".

   Result: The user gets an InvitationInfo object describing the invitation. In
   addition the server puts the client's ClientId into the Invitation object on
   the server. Thus from now on only this very client will have access to the
   invitation. No other device will now be able to view or accept the
   invitation.
   
2. The user does a delete on "invitation"/invitationid", with a reqbody
   containing Accept/Decline. Only the client previously called put on
   invitationInfo/... is allowed to do that. Result: Invitation will be deleted
   on the server. If reqbody contained Accept - the client is now part of the
   inviting family. On Decline - only the invitation gets deleted.
