module Gonimo.Server.State where


import Data.Map.Strict (Map)
import Control.Concurrent.STM.TVar


import Gonimo.Server.DbEntities (FamilyId)


data Family = Family {}

type FamilyVar = TVar Family
type FamilyMap = Map FamilyId FamilyVar

data State = State {
    families :: TVar FamilyMap
  }
