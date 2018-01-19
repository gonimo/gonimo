{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- SocketAPI/DB Translations ...
module Gonimo.Server.Db.IsDb where

import           Database.Persist.Sql
import           Control.Monad.State.Strict (StateT(..))

import Gonimo.SocketAPI.Types as API
import Gonimo.Db.Entities as Db

-- | Types that have a representation in the db.
class IsDbType apiType where
  type DbType apiType :: *
  fromDb :: DbType apiType -> apiType
  toDb   :: apiType -> DbType apiType


-- |  MonadState translation helper:
stateToDb :: (IsDbType apiType, Monad m) => StateT apiType m a -> StateT (DbType apiType) m a
stateToDb apiM = StateT $ \dbState -> do
  (a, s) <- runStateT apiM $ fromDb dbState
  pure (a, toDb s)



-- Key translations:

instance IsDbType API.DeviceId where
  type DbType API.DeviceId = Db.DeviceId
  fromDb = API.DeviceId . fromSqlKey
  toDb (API.DeviceId key) = toSqlKey key

instance IsDbType API.AccountId where
  type DbType API.AccountId = Db.AccountId
  fromDb = API.AccountId . fromSqlKey
  toDb (API.AccountId key) = toSqlKey key

instance IsDbType API.UserId where
  type DbType API.UserId = Db.UserId
  fromDb = API.UserId . fromSqlKey
  toDb (API.UserId key) = toSqlKey key

instance IsDbType API.FamilyId where
  type DbType API.FamilyId = Db.FamilyId
  fromDb = API.FamilyId . fromSqlKey
  toDb (API.FamilyId key) = toSqlKey key

instance IsDbType API.InvitationId where
  type DbType API.InvitationId = Db.InvitationId
  fromDb = API.InvitationId . fromSqlKey
  toDb (API.InvitationId key) = toSqlKey key

instance IsDbType API.FamilyAccountId where
  type DbType API.FamilyAccountId = Db.FamilyAccountId
  fromDb = API.FamilyAccountId . fromSqlKey
  toDb (API.FamilyAccountId key) = toSqlKey key

-- Table translations:
instance IsDbType API.Family where
  type DbType API.Family = Db.Family
  fromDb dbFam = API.Family { API.familyName              = Db.familyName dbFam
                            , API.familyCreated           = Db.familyCreated dbFam
                            , API.familyLastAccessed      = Db.familyLastAccessed dbFam
                            , API.familyLastUsedBabyNames = Db.familyLastUsedBabyNames dbFam
                            }

  toDb apiFam = Db.Family { Db.familyName              = API.familyName apiFam
                          , Db.familyCreated           = API.familyCreated apiFam
                          , Db.familyLastAccessed      = API.familyLastAccessed apiFam
                          , Db.familyLastUsedBabyNames = API.familyLastUsedBabyNames apiFam
                          }

instance IsDbType API.Device where
  type DbType API.Device = Db.Device
  fromDb dbFam = API.Device { API.deviceName = Db.deviceName dbFam
                            , API.deviceAuthToken = Db.deviceAuthToken dbFam
                            , API.deviceAccountId = fromDb $ Db.deviceAccountId dbFam
                            , API.deviceLastAccessed = Db.deviceLastAccessed dbFam
                            , API.deviceUserAgent = Db.deviceUserAgent dbFam
                            }

  toDb apiFam = Db.Device { Db.deviceName = API.deviceName apiFam
                          , Db.deviceAuthToken = API.deviceAuthToken apiFam
                          , Db.deviceAccountId = toDb $ API.deviceAccountId apiFam
                          , Db.deviceLastAccessed = API.deviceLastAccessed apiFam
                          , Db.deviceUserAgent = API.deviceUserAgent apiFam
                          }

instance IsDbType API.FamilyAccount where
  type DbType API.FamilyAccount = Db.FamilyAccount
  fromDb dbFam = API.FamilyAccount { API.familyAccountAccountId = fromDb $ Db.familyAccountAccountId dbFam
                                   , API.familyAccountFamilyId = fromDb $ Db.familyAccountFamilyId dbFam
                                   , API.familyAccountJoined = Db.familyAccountJoined dbFam
                                   , API.familyAccountInvitedBy = Db.familyAccountInvitedBy dbFam
                                   }

  toDb apiFam = Db.FamilyAccount  { Db.familyAccountAccountId = toDb $ API.familyAccountAccountId apiFam
                                  , Db.familyAccountFamilyId = toDb $ API.familyAccountFamilyId apiFam
                                  , Db.familyAccountJoined = API.familyAccountJoined apiFam
                                  , Db.familyAccountInvitedBy = API.familyAccountInvitedBy apiFam
                                  }

instance IsDbType API.Invitation where
  type DbType API.Invitation = Db.Invitation
  fromDb dbFam = API.Invitation { API.invitationSecret = Db.invitationSecret dbFam
                                , API.invitationFamilyId = fromDb $ Db.invitationFamilyId dbFam
                                , API.invitationCreated = Db.invitationCreated dbFam
                                , API.invitationDelivery = Db.invitationDelivery dbFam
                                , API.invitationSenderId = fromDb $ Db.invitationSenderId dbFam
                                , API.invitationReceiverId = fromDb <$> Db.invitationReceiverId dbFam
                                }

  toDb apiFam = Db.Invitation  { Db.invitationSecret = API.invitationSecret apiFam
                               , Db.invitationFamilyId = toDb $ API.invitationFamilyId apiFam
                               , Db.invitationCreated = API.invitationCreated apiFam
                               , Db.invitationDelivery = API.invitationDelivery apiFam
                               , Db.invitationSenderId = toDb $ API.invitationSenderId apiFam
                               , Db.invitationReceiverId = toDb <$> API.invitationReceiverId apiFam
                               }


instance IsDbType API.User where
  type DbType API.User = Db.User
  fromDb dbFam = API.User { API.userLogin = Db.userLogin dbFam
                          , API.userPassword = Db.userPassword dbFam
                          , API.userAccountId = fromDb $ Db.userAccountId dbFam
                          }

  toDb apiFam = Db.User { Db.userLogin = API.userLogin apiFam
                        , Db.userPassword = API.userPassword apiFam
                        , Db.userAccountId = toDb $ API.userAccountId apiFam
                        }

instance IsDbType API.Account where
  type DbType API.Account = Db.Account
  fromDb dbFam = API.Account { API.accountCreated = Db.accountCreated dbFam
                             }

  toDb apiFam = Db.Account { Db.accountCreated = API.accountCreated apiFam
                           }
