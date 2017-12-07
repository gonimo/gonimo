{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitForAll #-}
{-|
Module      : Gonimo.Server.Cache
Description : Short description
Copyright   : (c) Robert Klotzner, 2017
Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module Gonimo.Server.Cache ( -- * Types
                             Cache(..)
                             -- * Functions
                           , sampleAll
                           ) where

import Reflex
import Control.Lens

import Gonimo.Server.Cache.Internal


-- | Sample the whole cache for your convenience!
sampleAll :: forall t m. (Reflex t, MonadHold t m) => Cache t -> m Sampled
sampleAll cache' = do
  _sampledFamilies           <- sample $ cache'^.families
  _sampledInvitations        <- sample $ cache'^.invitations
  _sampledFamilyInvitations  <- sample $ cache'^.familyInvitations
  _sampledAccountInvitations <- sample $ cache'^.accountInvitations
  _sampledAccounts           <- sample $ cache'^.accounts
  _sampledFamilyAccounts     <- sample $ cache'^.familyAccounts
  _sampledAccountFamilies    <- sample $ cache'^.accountFamilies
  _sampledFamilyAccountData  <- sample $ cache'^.familyAccountData
  pure $ Sampled {..}


