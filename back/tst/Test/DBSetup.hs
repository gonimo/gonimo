{-# LANGUAGE OverloadedStrings #-}
module Test.DBSetup where

import           Gonimo.Server.Auth

import           Gonimo.Server.DbEntities
import           Gonimo.Server.Types
import           Database.Persist.Sqlite  as Db

import           Data.Time.Clock          (getCurrentTime)

setupDB :: IO AuthData
setupDB = do
  now <- getCurrentTime
  runSqlite "WAL=off :memory:" $ do
     runMigration migrateAll
     fid <- Db.insert Family { familyName = "gonimo"
                             , familyCreated = now
                             , familyLastAccessed = now }

     fromA@(Entity fromAID _ ) <- Db.insertEntity Account {accountCreated = now}

     fromC   <- Db.insertEntity Client { clientName = "TestFrom"
                                       , clientAuthToken = GonimoSecret (Secret "secret")
                                       , clientAccountId = fromAID
                                       , clientLastAccessed = now
                                       , clientUserAgent = "TestUserAgent"
                                       }

     return AuthData{ _accountEntity = fromA
                    , _allowedFamilies = [fid]
                    , _clientEntity = fromC}

