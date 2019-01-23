{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenNode.Data
  ( ResponseData (..)
  , WithdrawalType (..)
  , Withdrawal (..)
  )
where

import           Data.Aeson
import           Data.Char
import           GHC.Generics

aesonOptions = defaultOptions { constructorTagModifier = map toLower }

newtype ResponseData a = ResponseData {responseData :: a} deriving (Show, Eq)

data WithdrawalType = Chain | Ln | Exchange | Wire deriving (Show, Eq, Generic)

data Withdrawal =
  Withdrawal { wdId :: String
             , wdAmount :: Integer
             , wdType :: WithdrawalType
             , wdReference :: String
             , wdProcessedAt :: Maybe Integer
             , wdStatus :: String
             , wdAddress :: String
             , wdFee :: Integer
             , wdFiatValue :: Maybe Integer
             } deriving (Show, Eq, Generic)


instance FromJSON a => FromJSON (ResponseData a) where
  parseJSON = withObject "ResponseData" $ \v -> ResponseData <$> (v .: "data")

instance ToJSON a => ToJSON (ResponseData a) where
  toJSON (ResponseData d) = object ["data" .= d]

instance ToJSON WithdrawalType where
  toJSON     = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions

instance FromJSON WithdrawalType where
  parseJSON = genericParseJSON aesonOptions

instance FromJSON Withdrawal where
  parseJSON = withObject "Withdrawal" $ \v ->
    Withdrawal
      <$> v .:  "id"
      <*> v .:  "amount"
      <*> v .:  "type"
      <*> v .:  "reference"
      <*> v .:? "processed_at"
      <*> v .:  "status"
      <*> v .:  "address"
      <*> v .:  "fee"
      <*> v .:? "fiat_value"

instance ToJSON Withdrawal where
  toJSON (Withdrawal i am ty re pr st ad fe fi) = object
    [ "id" .= i
    , "amount" .= am
    , "type" .= ty
    , "reference" .= re
    , "processed_at" .= pr
    , "status" .= st
    , "address" .= ad
    , "fee" .= fe
    , "fiat_value" .= fi
    ]
