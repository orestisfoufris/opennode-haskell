{-# LANGUAGE OverloadedStrings #-}

module OpenNode.Api
  ( withdrawalInfo
  , withdrawals
  , exchangeRates
  , charges
  , chargeInfo
  , createCharge
  , accountBalance
  , supportedCurrencies
  , initiateWithdrawal
  , listRefunds
  )
where

import           Data.Aeson         (Object)
import           Data.Default.Class ()
import           Data.Monoid        ((<>))
import           Data.Text          (Text)
import           Network.HTTP.Req
import           OpenNode.Config
import           OpenNode.Data

withdrawalInfo :: (MonadHttp m) => Config -> Text -> m (ResponseData Withdrawal)
withdrawalInfo cfg wid = responseBody <$> req
  GET
  (https baseUrl /: path /: wid)
  NoReqBody
  jsonResponse
  (header "Authorization" token)
 where
  path    = "/v1/withdrawal/"
  baseUrl = configUrl cfg
  token   = configToken cfg

withdrawals :: (MonadHttp m) => Config -> m (ResponseData [Withdrawal])
withdrawals cfg = responseBody <$> req
  GET
  (https baseUrl /: "v1" /: "withdrawals")
  NoReqBody
  jsonResponse
  (header "Authorization" token <> header "Content-Type" "application/json")
 where
  baseUrl = configUrl cfg
  token   = configToken cfg

initiateWithdrawal :: (MonadHttp m) => Config -> WithdrawalRequest
                 -> m (ResponseData WithdrawalResponse)
initiateWithdrawal cfg wreq =
  responseBody <$>
  req
    POST
    (https baseUrl /: "v2" /: "withdrawals")
    (ReqBodyJson wreq)
    jsonResponse
    (header "Authorization" token)
  where
    baseUrl = configUrl cfg
    token = configToken cfg

charges :: (MonadHttp m) => Config -> m (ResponseData [Charge])
charges cfg = responseBody <$> req
  GET
  (https baseUrl /: "v1" /: "charges")
  NoReqBody
  jsonResponse
  (header "Authorization" token)
  where
    baseUrl = configUrl cfg
    token = configToken cfg

chargeInfo:: (MonadHttp m) => Config -> Text -> m (ResponseData Charge)
chargeInfo cfg cid = responseBody <$> req
  GET
  (https baseUrl /: "v1" /: "charge" /: cid)
  NoReqBody
  jsonResponse
  (header "Authorization" token)
  where
    path = "/v1/charge/"
    baseUrl = configUrl cfg
    token = configToken cfg

createCharge :: (MonadHttp m) => Config -> ChargeRequest
             -> m (ResponseData ChargeResponse)
createCharge cfg creq = responseBody <$> req
  POST
  (https baseUrl /: "v1" /: "charges")
  (ReqBodyJson creq)
  jsonResponse
  (header "Authorization" token)
  where
    path = "/v1/charges"
    baseUrl = configUrl cfg
    token = configToken cfg

accountBalance :: (MonadHttp m) => Config -> m (ResponseData Balance)
accountBalance cfg = responseBody <$> req
  GET
  (https baseUrl /: "v1" /: "account" /: "balance")
  NoReqBody
  jsonResponse
  (header "Authorization" token)
  where
    baseUrl = configUrl cfg
    token = configToken cfg

exchangeRates :: (MonadHttp m) => Config -> m (ResponseData Rates)
exchangeRates cfg = responseBody <$> req
  GET
  (https baseUrl /: "v1" /: "rates")
  NoReqBody
  jsonResponse
  (header "Authorization" token)
  where
    path = "/v1/rates"
    baseUrl = configUrl cfg
    token = configToken cfg

supportedCurrencies :: (MonadHttp m) => Config -> m (ResponseData [Text])
supportedCurrencies cfg = responseBody <$> req
  GET
  (https baseUrl /: "v1" /: "currencies")
  NoReqBody
  jsonResponse
  (header "Authorization" token)
  where
    baseUrl = configUrl cfg
    token = configToken cfg

listRefunds :: (MonadHttp m) => Config -> m (ResponseData [Refund])
listRefunds cfg = responseBody <$> req
  GET
  (https baseUrl /: "v1" /: "refunds")
  NoReqBody
  jsonResponse
  (header "Authorization" token)
  where
    baseUrl = configUrl cfg
    token = configToken cfg
