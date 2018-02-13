{-|
Module      : Gonimo.Client.Account
Description : Account specific data is served by this module.
Copyright   : (c) Robert Klotzner, 2018
At the moment this is only about managing of claimed invitations.
-}
module Gonimo.Client.Account ( -- * Types
                               Config(..)
                             , HasConfig(..)
                             , Account(..)
                             , HasAccount(..)
                              -- * Creation
                             , make
                             ) where



import Gonimo.Client.Account.Internal



-- | Create an Account.
--
--   You can claim invitations and answer them.
--   At the moment we don't yet have a means for getting claimed invitations
--   from the server (ReqGetClaimedInvitations), so we only provide the ones of
--   the current session. If you restart gonimo, your claimed invitations are no
--   longer visible.
make :: (Reflex t, MonadHold t m, MonadFix m) => Config t -> m Account
make conf = do
  _claimedInvitations <- makeClaimedInvitations conf

  let
    _serverConfig = makeServerConfig conf

  pure $ Account {..}
