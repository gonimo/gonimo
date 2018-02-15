{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Gonimo.Client.Account
Description : Account specific data is served by this module.
Copyright   : (c) Robert Klotzner, 2018
At the moment this is only about managing of claimed invitations.
-}
module Gonimo.Client.Account ( -- * Interface
                               module API
                               -- * Types
                             , Model(..)
                             , _server
                             , FullAccount(..)
                             , _account
                             , serverConfig
                               -- * Creation
                             , make
                             ) where



import           Gonimo.Client.Account.API      as API
import           Gonimo.Client.Account.Internal
import           Gonimo.Client.Prelude



-- | Create an Account.
--
--   You can claim invitations and answer them.
--   At the moment we don't yet have a means for getting claimed invitations
--   from the server (ReqGetClaimedInvitations), so we only provide the ones of
--   the current session. If you restart gonimo, your claimed invitations are no
--   longer visible.
make :: (Reflex t, MonadHold t m, MonadFix m, HasModel d, HasConfig c)
  => d t -> c t -> m (FullAccount t)
make model conf = do
  _claimedInvitations <- makeClaimedInvitations model
  _subscriberConfig   <- subscribeInvitationClaims conf
  let
    _serverConfig = answerInvitations conf

  _subscriberConfig <- subscribeInvitationClaims conf

  pure $ FullAccount { __account = Account {..}
                     , ..
                     }
