{-|
Module      : Gonimo.Server.Cache.Invitations
Description : Multiple indexed table for 'Invitation'
Copyright   : (c) Robert Klotzner, 2017
-}
module Gonimo.Server.Cache.Invitations ( Invitations
                                       , make
                                       , byFamilyId
                                       , byReceiverId
                                       ) where


import           Data.Map                         (Map)

import           Gonimo.Server.Cache.IndexedTable as Table
import           Gonimo.SocketAPI.Model



type ReceiverIndexed = IndexedTable AccountId Map

type Invitations = IndexedTable FamilyId ReceiverIndexed InvitationId Invitation

-- | Create a new Invitations indexed data structure from a raw Map
make :: Map InvitationId Invitation -> Invitations
make invitations' = fromRawTable (Just . invitationFamilyId) receiverIndexed
  where
    receiverIndexed = fromRawTable invitationReceiverId invitations'


-- | Serch entries by FamilyId
byFamilyId :: Invitations -> Map FamilyId [InvitationId]
byFamilyId = getIndex


byReceiverId :: Invitations -> Map AccountId [InvitationId]
byReceiverId = getIndex . Table.getInner
