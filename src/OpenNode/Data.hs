{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveAnyClass        #-}

module OpenNode.Data
  ( ResponseData (..)
  , WithdrawalType (..)
  , Withdrawal (..)
  , Charge (..)
  , ChargeRequest (..)
  , ChargeResponse (..)
  , Balance (..)
  , Rate (..)
  , Rates (..)
  , WithdrawalRequest (..)
  , WithdrawalResponse (..)
  )
where

import           Data.Aeson
import           Data.Char
import qualified Data.Map        as M
import           Data.Maybe
import           Data.Scientific
import           Data.Text       (Text)
import           GHC.Generics


aesonOptions :: Options
aesonOptions = defaultOptions { constructorTagModifier = map toLower }

newtype ResponseData a = ResponseData { responseData :: a } deriving (Show, Eq)

instance FromJSON a => FromJSON (ResponseData a) where
  parseJSON = withObject "ResponseData" $ \v -> ResponseData <$> (v .: "data")

instance ToJSON a => ToJSON (ResponseData a) where
  toJSON (ResponseData d) = object ["data" .= d]

-- '/withdrawal'  data

data WithdrawalType = Chain | Ln | Exchange | Wire deriving (Show, Eq, Generic)

data Withdrawal =
  Withdrawal { wdId          :: String
             , wdAmount      :: Integer
             , wdType        :: WithdrawalType
             , wdReference   :: String
             , wdProcessedAt :: Maybe Integer
             , wdStatus      :: String
             , wdAddress     :: String
             , wdFee         :: Integer
             , wdFiatValue   :: Maybe Integer
             } deriving (Show, Eq, Generic)

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

-- '/charge'  data

data Charge =
  Charge { id       :: String
  , name            :: Maybe String
  , description     :: String
  , amount          :: Integer
  , status          :: ChargeState
  , createdAt       :: Integer
  , fee             :: Maybe Integer
  , fiatValue       :: Double
  , notes           :: String
  , autoSettle      :: Bool
  , chainInvoice    :: Maybe ChainInvoice
  , lightingInvoice :: Maybe LightingInvoice
} deriving (Show, Eq, Generic)

instance FromJSON Charge where
  parseJSON = withObject "Charge" $ \v ->
    Charge
      <$> v .:  "id"
      <*> v .:?  "name"
      <*> v .:  "description"
      <*> v .:  "amount"
      <*> v .:  "status"
      <*> v .:  "created_at"
      <*> v .:?  "fee"
      <*> v .:  "fiat_value"
      <*> v .:  "notes"
      <*> v .:  "auto_settle"
      <*> v .:?  "chain_invoice"
      <*> v .:?  "lighting_invoice"

instance ToJSON Charge where
  toJSON (Charge cid na de am st ca fe fv no au ci li ) = object
    [ "id" .= cid
    , "name" .= na
    , "description" .= de
    , "amount" .= am
    , "status" .= st
    , "created_at" .= ca
    , "fee" .= fe
    , "fiat_value" .= fv
    , "notes" .= no
    , "auto_settle" .= au
    , "chain_invoice" .= ci
    , "lighting_invoice" .= li
    ]

data ChargeState = Paid | Processing | Unpaid deriving (Eq, Show, Generic)

instance ToJSON ChargeState where
  toJSON     = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions

instance FromJSON ChargeState where
  parseJSON = genericParseJSON aesonOptions

data ChainInvoice =
  ChainInvoice { address :: String
  , settledAt            :: Maybe Integer
  , tx                   :: Maybe String
} deriving (Show, Eq, Generic)

instance FromJSON ChainInvoice where
  parseJSON = withObject "ChainInvoice" $ \v ->
    ChainInvoice
      <$> v .:  "address"
      <*> v .:?  "settled_at"
      <*> v .:?  "tx"

instance ToJSON ChainInvoice where
  toJSON (ChainInvoice ad se t) = object
    [ "address" .= ad , "settled_at" .= se , "tx" .= t ]

data LightingInvoice =
  LightingInvoice { lsettledAt :: Integer
  , payreq                     :: String
  } deriving (Show, Eq, Generic)

instance FromJSON LightingInvoice where
  parseJSON = withObject "LightingInvoice" $ \v ->
    LightingInvoice
      <$> v .:  "settled_at"
      <*> v .:  "payreq"

instance ToJSON LightingInvoice where
  toJSON (LightingInvoice se pr) = object
    [ "settled_at" .= se , "payreq" .= pr ]

-- /createCharge

-- / TODO: DO NOT USE DOUBLE. Move that at least to Scientific
-- / For what the docs say currency seems optional
data ChargeRequest =
  ChargeRequest { description :: String
 , amount                     :: Double
 , currency                   :: String
 , orderId                    :: Maybe String
 , customerEmail              :: String
 , customerName               :: Maybe String
 , callbackUrl                :: Maybe String
 , successUrl                 :: Maybe String
 , autoSettle                 :: Maybe Bool
} deriving (Show, Eq, Generic)

instance ToJSON ChargeRequest where
  toJSON (ChargeRequest de am cur oid ce cn cu su as ) = object
    [ "description" .= de
    , "amount" .= am
    , "currency" .= cur
    , "order_id" .= oid
    , "customer_email" .= ce
    , "customer_name" .= cn
    , "callback_url" .= cu
    , "success_url" .= su
    , "auto_settle" .= as
    ]

data WithdrawalRequest = WithdrawalRequest
  { wrType         :: WithdrawalType
  , wrAmount       :: Integer
  , wrAddress      :: String
  , wrCallback_url :: String
  } deriving (Show, Eq, Generic)

instance ToJSON WithdrawalRequest where
  toJSON (WithdrawalRequest ty am ad ca) =
    object
      ["type" .= ty, "amount" .= am, "address" .= ad, "callback_url" .= ca]

data ChargeResponse =
  ChargeResponse {  id :: String
  , name               :: Maybe String
  , description        :: String
  , amount             :: Maybe Integer
  , status             :: Maybe ChargeState
  , createdAt          :: Maybe Integer
  , fiatValue          :: Maybe Double
  , notes              :: Maybe String
  , callbackUrl        :: Maybe String
  , successUrl         :: Maybe String
  , orderId            :: Maybe String
  , currency           :: Maybe String
  , sourceFiatValue    :: Maybe Double
  , autoSettle         :: Maybe Bool
  , chainInvoice       :: Maybe ChainInvoice
  , lightingInvoice    :: Maybe LightingInvoice
 } deriving (Show, Eq, Generic)

instance ToJSON ChargeResponse where
  toJSON (ChargeResponse ide na de am st ca fv no cu su oid cur sfv as ci li) = object
    [ "id" .= ide
    , "name" .= na
    , "description" .= de
    , "amount" .= am
    , "status" .= st
    , "created_at" .= ca
    , "fiat_value" .= fv
    , "notes" .= no
    , "callback_url" .= cu
    , "success_url" .= su
    , "order_id" .= oid
    , "currency" .= cur
    , "source_fiat_value" .= sfv
    , "auto_settle" .= as
    , "chain_invoice" .= ci
    , "lighting_invoice" .= li
    ]

instance FromJSON ChargeResponse where
  parseJSON = withObject "ChargeResponse" $ \v ->
    ChargeResponse
          <$> v .:  "id"
          <*> v .:?  "name"
          <*> v .: "description"
          <*> v .:?  "amount"
          <*> v .:?  "status"
          <*> v .:?  "created_at"
          <*> v .:?  "fiat_value"
          <*> v .:?  "notes"
          <*> v .:?  "callback_url"
          <*> v .:?  "success_url"
          <*> v .:?  "order_id"
          <*> v .:?  "currency"
          <*> v .:?  "source_fiat_value"
          <*> v .:?  "auto_settle"
          <*> v .:?  "chain_invoice"
          <*> v .:?  "lighting_invoice"

data WithdrawalResponse = WithdrawalResponse
 {  wsId                 :: String
  , wsType               :: Maybe WithdrawalType
  , wsAmount             :: Maybe Integer
  , wsReference          :: Maybe String
  , wsProcessedAt        :: Maybe Integer
  , wsFee                :: Maybe Integer
  } deriving (Show, Eq, Generic)

instance ToJSON WithdrawalResponse where
  toJSON (WithdrawalResponse ide ty am re pr fe) = object
    [ "id" .= ide
    , "type" .= ty
    , "amount" .= am
    , "reference" .= re
    , "processed_at" .= pr
    , "fee" .= fe
    ]

instance FromJSON WithdrawalResponse where
  parseJSON = withObject "WithdrawalResponse" $ \v ->
    WithdrawalResponse
      <$> v .:   "id"
      <*> v .:?  "type"
      <*> v .:?  "amount"
      <*> v .:?  "reference"
      <*> v .:?  "processed_at"
      <*> v .:?  "fee"

data Balance = Balance
  { balance :: M.Map Text Scientific
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Rate = Rate (M.Map Text Scientific)
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Rates = Rates (M.Map Text Rate)
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
