{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans      #-}

module Gonimo.Db.PersistFields () where

import           Database.Persist.Class (PersistField, fromPersistValue,
                                         toPersistValue)
import           Database.Persist.Sql   (PersistFieldSql, sqlType)
import           Database.Persist.TH
import           Database.Persist.Types (PersistValue (PersistText),
                                         SqlType (SqlString))

import           Gonimo.SocketAPI.Types (InvitationDelivery)
import           Gonimo.Types

derivePersistField "Secret"
derivePersistField "AuthToken"
derivePersistField "InvitationDelivery"

instance PersistField FamilyName where
  toPersistValue = PersistText . writeFamilyName
  fromPersistValue (PersistText t) = Right (parseFamilyName t)
  fromPersistValue _               = Left "A FamilyName must be PersistText"

instance PersistFieldSql FamilyName where
  sqlType _ = SqlString
