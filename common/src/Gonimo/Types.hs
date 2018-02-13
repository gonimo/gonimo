{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Gonimo.Types where




import           Control.Error.Safe     (rightZ)

import           Data.Aeson.Types       (FromJSON (..), FromJSON, ToJSON (..),
                                         ToJSON (..), Value (String),
                                         defaultOptions, genericToJSON, genericToEncoding, typeMismatch)
import Data.Aeson
import Control.Applicative ((<|>))

import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base64 as Base64
import           Data.Monoid
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)

import           GHC.Generics           (Generic)
import           Control.Monad          (MonadPlus, mzero)
import qualified Data.Text      as T
import           Data.Text      (Text)
import           Control.Lens hiding ((.=))

data DeviceType = NoBaby
                | Baby Text
                deriving (Show, Eq, Ord, Generic)


_NoBaby :: Prism' DeviceType ()
_NoBaby = let f () = NoBaby
              g NoBaby = Right ()
              g x      = Left x
           in prism f g

_Baby :: Prism' DeviceType Text
_Baby = let g (Baby y) = Right y
            g x        = Left x
         in prism Baby g


toBabyName :: MonadPlus m => DeviceType -> m Text
toBabyName NoBaby = mzero
toBabyName (Baby name) = pure name

instance FromJSON DeviceType
instance ToJSON DeviceType

type SenderName = Text

newtype Secret = Secret ByteString
  deriving (Generic, Show, Read, Ord, Eq)

instance FromJSON Secret where
  parseJSON (String t) = Secret <$> (rightZ . Base64.decode . encodeUtf8 $ t)
  parseJSON _ = fail "Expecting a string when parsing a secret."

instance ToJSON Secret where
  toJSON (Secret bs) = String . decodeUtf8 . Base64.encode $ bs
  toEncoding (Secret bs) = toEncoding $ (decodeUtf8 . Base64.encode) bs

-- | TODO: More type safety for InvitationSecret, roadmap:
--   1. [x] Introduce type synonym InvitationSecret
--   2. [ ] Use InvitationSecret everywhere instead of Secret (where applicable)
--   3. [ ] Make it a newtype and fix breaking code
type InvitationSecret = Secret

-- Other auth methods might be added later on, like oauth bearer tokens:
data AuthToken = GonimoSecret Secret
               | PlaceHolder____
               deriving (Read, Show, Generic, Eq, Ord)

instance FromJSON AuthToken
instance ToJSON AuthToken where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

data Coffee = Tea deriving Generic
instance FromJSON Coffee
instance ToJSON Coffee where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions



data FamilyName
  = FamilyName { familyMemberName :: !Text
               , familyNameName :: !Text
               } deriving (Show, Generic, Eq)

parseFamilyName :: Text -> FamilyName
parseFamilyName t =
  let
    parseList :: [Text] -> Maybe FamilyName
    parseList [ mN, fN ] = Just $ FamilyName mN fN
    parseList _ = Nothing

    parseLine = parseList . map T.strip . T.splitOn ","
  in
    case parseLine t of
      Nothing -> FamilyName t t
      Just f  -> f

writeFamilyName :: FamilyName -> Text
writeFamilyName (FamilyName mN fN) =
  if mN == fN
  then mN
  else mN <> ", " <> fN

-- Custom instances necessary for backwards compatiblity (familyName got changed to familyNameName).
-- Roadmap:
--  - [x] Encode like it used to be ("familyName") and accept both: ("familyName" & "familyNameName")
--  - [ ] Use new encoding, still accept both
--  - [ ] use generic impl again.
instance FromJSON FamilyName where
  parseJSON (Object v) = try1 <|> try2
    where
      try1 = FamilyName
        <$> v .: "familyMemberName"
        <*> v .: "familyName"
      try2 = FamilyName
        <$> v .: "familyMemberName"
        <*> v .: "familyNameName"
  parseJSON invalid    = typeMismatch "FamilyName" invalid


instance ToJSON FamilyName where
  -- toJSON = genericToJSON defaultOptions
  toJSON name = object [ "familyName" .= familyNameName name, "familyMemberName" .= familyMemberName name  ]
  -- toEncoding = genericToEncoding defaultOptions
  toEncoding name = pairs ( "familyName" .= familyNameName name <> "familyMemberName" .= familyMemberName name  )



--------------------------------------------------

type EmailAddress = Text

data InvitationDelivery = EmailInvitation EmailAddress
                        | OtherDelivery
                        deriving (Read, Show, Eq, Ord, Generic)

instance FromJSON InvitationDelivery

instance ToJSON InvitationDelivery where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions
