{-|
Module      : Gonimo.Server.Cache.FamilyAccounts
Description : Double indexed table for 'FamilyAccount'
Copyright   : (c) Robert Klotzner, 2017
-}
module Gonimo.Server.Cache.FamilyAccounts where


import           Data.Map                         (Map)
import           Data.Set                         (Set)
import qualified Data.Set as Set
import           Control.Lens
import           Control.Applicative

import           Gonimo.Server.Cache.IndexedTable as Table
import           Gonimo.SocketAPI.Model


type FamilyAccounts = IndexedTable FamilyId (IndexedTable AccountId Map) FamilyAccountId FamilyAccount

-- | Create a new FamilyAccounts indexed data structure from a raw Map
make :: Map FamilyAccountId FamilyAccount -> FamilyAccounts
make accounts' = fromRawTable (Just . familyAccountFamilyId) inner
  where
    inner = fromRawTable (Just . familyAccountAccountId) accounts'


-- | Search entries by AccountId
byAccountId :: FamilyAccounts -> Map AccountId (Set FamilyAccountId)
byAccountId = getIndex . Table.getInner

-- | Serch entries by FamilyId
byFamilyId :: FamilyAccounts -> Map FamilyId (Set FamilyAccountId)
byFamilyId = getIndex

findByFamilyAndAccountId :: FamilyId -> AccountId -> FamilyAccounts -> Maybe FamilyAccountId
findByFamilyAndAccountId fid aid famAccs = do
    rAccount <- byAccountId famAccs ^. at aid
    rFamily <- byFamilyId famAccs ^. at fid
    let r = Set.intersection rAccount rFamily
    toMaybe r
  where
    toMaybe = foldr (\x -> (Just x <|>)) Nothing


-- | Get all account family members
getAccounts :: FamilyId -> FamilyAccounts -> [AccountId]
getAccounts fid = map getAccount . Table.lookupByIndex fid
  where
    getAccount = familyAccountAccountId . snd

-- | Get the families of an account
getFamilies :: AccountId -> FamilyAccounts -> [FamilyId]
getFamilies aid = map getFamily . Table.lookupByIndex aid . Table.getInner
  where
    getFamily = familyAccountFamilyId . snd
