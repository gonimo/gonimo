{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -fno-warn-orphans      #-}

module Gonimo.Types.Extended (module Gonimo.Types) where

import           Database.Persist.Class (PersistField, toPersistValue, fromPersistValue)
import           Database.Persist.Sql   (PersistFieldSql, sqlType)
import           Database.Persist.Types (PersistValue(PersistText), SqlType(SqlString))
import           Database.Persist.TH

import Gonimo.Types

derivePersistField "Secret"
derivePersistField "AuthToken"
derivePersistField "InvitationDelivery"

instance PersistField FamilyName where
  toPersistValue = PersistText . writeFamilyName
  fromPersistValue (PersistText t) = Right (parseFamilyName t)
  fromPersistValue _ = Left "A FamilyName must be PersistText"

instance PersistFieldSql FamilyName where
  sqlType _ = SqlString
