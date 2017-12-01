{-|
Module      : Gonimo.Server.Config.Internal
Description : Types and internal functions for "Gonimo.Server.Config"
Copyright   : (c) Robert Klotzner, 2017
-}
module Gonimo.Server.Config.Internal where

import           Control.Concurrent.STM      (TVar)
import           Control.Lens
import           Crypto.Random               (SystemRandom)
import           Data.Pool                   (Pool)
import           Data.Text                   (Text)
import           Database.Persist.Sql        (SqlBackend)

import           Gonimo.Server.NameGenerator (FamilyNames, Predicates)





type DbPool = Pool SqlBackend

data Config
  = Config { _dbPool      :: !DbPool
           , _familyNames :: !FamilyNames
           , _predicates  :: !Predicates
           , _frontendURL :: !Text
           , _random      :: !(TVar SystemRandom)
           }



-- Lenses:

class HasConfig a where
  config :: Lens' a Config

  dbPool :: Lens' a DbPool
  dbPool = config . go
    where
      go :: Lens' Config DbPool
      go f config' = (\dbPool' -> config' { _dbPool = dbPool' }) <$> f (_dbPool config')


  familyNames :: Lens' a FamilyNames
  familyNames = config . go
    where
      go :: Lens' Config FamilyNames
      go f config' = (\familyNames' -> config' { _familyNames = familyNames' }) <$> f (_familyNames config')


  predicates :: Lens' a Predicates
  predicates = config . go
    where
      go :: Lens' Config Predicates
      go f config' = (\predicates' -> config' { _predicates = predicates' }) <$> f (_predicates config')


  frontendURL :: Lens' a Text
  frontendURL = config . go
    where
      go :: Lens' Config Text
      go f config' = (\frontendURL' -> config' { _frontendURL = frontendURL' }) <$> f (_frontendURL config')


  random :: Lens' a ((TVar SystemRandom))
  random = config . go
    where
      go :: Lens' Config ((TVar SystemRandom))
      go f config' = (\random' -> config' { _random = random' }) <$> f (_random config')


instance HasConfig Config where
  config = id

