{-|
Module      : Gonimo.Server.Cache.Invitations
Description : Multiple indexed table for 'Invitation'
Copyright   : (c) Robert Klotzner, 2017
-}
module Gonimo.Server.Cache.Invitations ( Invitations
                                       , make
                                       , bySecret
                                       , byFamilyId
                                       , byReceiverId
                                       ) where


import           Data.Map                         (Map)

import           Gonimo.Server.Cache.IndexedTable as Table
import           Gonimo.SocketAPI.Model


type SecretIndexed = IndexedTable Secret Map

type ReceiverIndexed = IndexedTable AccountId SecretIndexed

type Invitations = IndexedTable FamilyId ReceiverIndexed InvitationId Invitation

-- | Create a new Invitations indexed data structure from a raw Map
make :: Map InvitationId Invitation -> Invitations
make invitations' = fromRawTable (Just . invitationFamilyId) receiverIndexed
  where
    secretIndexed = fromRawTable (Just . invitationSecret) invitations'
    receiverIndexed = fromRawTable invitationReceiverId secretIndexed


-- | Search entries by Secret
bySecret :: Invitations -> Map Secret [InvitationId]
bySecret = getIndex . Table.getInner . Table.getInner

-- | Serch entries by FamilyId
byFamilyId :: Invitations -> Map FamilyId [InvitationId]
byFamilyId = getIndex


byReceiverId :: Invitations -> Map AccountId [InvitationId]
byReceiverId = getIndex . Table.getInner
