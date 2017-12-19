{-|
Module      : Gonimo.Server.Handler
Description : Handle incoming requests.
Copyright   : (c) Robert Klotzner, 2017

Notice: For db writing applications don't use the cache! If you need data
retrieve it directly from the db, this is needed to ensure consistency. Cache
and database don't need to be in sync.
-}
module Gonimo.Server.Handler ( -- * Types
                             , Handler(..)
                             ) where



import Gonimo.Server.Handler.Internal



handleRequest :: Server.Config -> Request -> IO RequestResult
handleRequest conf req
  = case req^.message of
    Ping                     -> pure mempty
    MakeDevice _             -> pure mempty
    Authenticate _           -> pure mempty
    MakeFamily               -> makeFamily conf
    MakeInvitation fid       -> denyUnless (isOnlineInFamily clients' senderId fid)
    -- If the device knows the secret it may access the invitation. Whether it is already
    -- claimed, is checked at access:
    ClaimInvitation _        -> mzero
    AnswerInvitation invId _ -> denyUnless (deviceOwnsInvitation cache' senderId invId)
    SendMessage toId _       -> denyUnless (isOnlineInSameFamily clients' senderId toId)
    UpdateServer update'     -> denyUpdate auth update'
    Get view'                -> denyView auth view'


-- | Create a new family in the database and update the cache accordingly.
makeFamily :: Server.Config -> Request -> RequestResult
makeFamily conf req = mempty & set actions $ do
  now <- getCurrentTime
  let predPool = conf ^. predicates
  let namePool = conf ^. familyNames
  n <- generateFamilyName predPool namePool
  runDb $ do
    -- Don't use cache!!
    aid <- deviceAccountId <$> Device.get (req ^. senderId)
    let family' = Family {
        API.familyName = n
      , familyCreated = now
      , familyLastAccessed = now
      , familyLastUsedBabyNames = []
    }

    fid <- Family.insert family'
    let familyAccount' = FamilyAccount {
        familyAccountAccountId = aid
      , familyAccountFamilyId = fid
      , familyAccountJoined = now
      , familyAccountInvitedBy = Nothing
    }

    Account.joinFamily predPool familyAccount'

    pure $ RequestResult { _responses =  }
