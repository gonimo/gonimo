module Gonimo.Error where


import           Data.Aeson
import           GHC.Generics
-- Define an error types, so handling errors is easier at the client side.
-- This makes it easier to handle them at the client side.

data Error =
  InvalidAuthToken
  deriving (Generic)

instance ToJSON Error where
    toJSON x = genericToJSON defaultOptions x
