{-|
Module      : Gonimo.Server.Handler.Internal
Description : Types and internal functions for "Gonimo.Server.Handler"
Copyright   : (c) Robert Klotzner, 2017
-}
module Gonimo.Server.Handler.Internal where


data Request
  = Request { __model :: Cache.Model
            , _statuses :: ClientStatuses
            , _message :: FromClient
            , _senderId :: DeviceId
            }

-- data RequestResult t
--   = RequestResult = { _responses :: Event t [(DeviceId, ToClient)]
--                     , _cacheUpdate :: Cache.Config t
--                     , _dbWrites :: Event t [ReaderT SqlBackend IO a]
--                     }
data RequestResult
  = RequestResult = { _responses :: [(DeviceId, ToClient)]
                    , _cacheUpdate :: Model -> Model
                    , _dbWrites :: [ReaderT SqlBackend IO ()]
                      -- Some requests require some IO which will be performed asynchronously.
                    , _dbRequests :: [Db.Request]
                    }

-- | Convenience function for setting '_action'
--
--   Usage:
--
-- @
--   setAction mempty $ do
--     myAction code
-- @
setAction :: RequestResult -> IO RequestResult
setAction result' action' = mempty & action .~ action'


instance HasClientStatuses Request
  clientStatuses f a = (\clientStatuses' -> a { _statuses = clientStatuses' }) <$> f (_statuses a)

instance Monoid RequestResult where
  mempty = RequestResult [] id []
  mappend (RequestResult r1 c1 db1 a1) (RequestResult r2 c2 db2 a2)
    = RequestResult (r1 <> r2) (c1 . c2) (db1 <> db2) (a1 <> a2)
