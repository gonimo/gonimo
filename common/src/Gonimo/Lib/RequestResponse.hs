{-|
Module      : Gonimo.Lib.RequestResponse
Description : Common data type for tracke requests and responses.
Copyright   : (c) Robert Klotzner, 2017

Sometimes you have requests and responses travelling through the FRP framework
and you have to map a response to a previously sent request in some way. This
data type lays out the foundation for this purpose. It provides a simple tuple
like data type with a requester and some payload.
-}
module Gonimo.Lib.RequestResponse where

import Control.Lens

-- | A common type for requests and responses.
--
--   r .. is the requester, a type that gets passed to requests and will be returned by responses. It can be chosen freely by the user, for the purpose of mapping responses to sent requests.
--
--   p .. The payload, it will either be the actual request for a request or the response for a response message.
data RequestResponse r p = RequestResponse r p

-- | Get access to the requester.
requester :: Lens (RequestResponse r1 p) (RequestResponse r2 p) r1 r2
requester f (RequestResponse r p) = (\newR -> RequestResponse newR p) <$> f r

-- | Access the payload.
--
--   You are assumed to use your own more descriptive aliases for payload, like
--   result or command or something simmilar. Otherwise you could have used a plain tuple as well ;-)
payload :: Lens' (RequestResponse r p) p
payload f (RequestResponse r p) = (\newP -> RequestResponse r newP) <$> f p
