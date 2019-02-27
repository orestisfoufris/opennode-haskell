{-# LANGUAGE OverloadedStrings #-}

module OpenNode.Api
  ( withdrawal
  , withdrawals
  , exchangeRates
  , charges
  , charge
  , createCharge
  )
where

import           Data.Aeson         (Object)
import           Data.Default.Class ()
import           Data.Monoid        ((<>))
import           Data.Text          (Text)
import           Network.HTTP.Req
import           OpenNode.Config
import           OpenNode.Data

withdrawal :: (MonadHttp m) => Config -> Text -> m (ResponseData Withdrawal)
withdrawal cfg wid = responseBody <$> req
  GET
  (https baseUrl /: path /: wid)
  NoReqBody
  jsonResponse
  (header "Authorization" token <> header "Accept" "application/json")
 where
  path    = "/v1/withdrawal/"
  baseUrl = configUrl cfg
  token   = configToken cfg

withdrawals :: (MonadHttp m) => Config -> m (ResponseData [Withdrawal])
withdrawals cfg = responseBody <$> req
  GET
  (https baseUrl /: path)
  NoReqBody
  jsonResponse
  (header "Authorization" token <> header "Accept" "application/json")
 where
  path    = "/v1/withdrawals"
  baseUrl = configUrl cfg
  token   = configToken cfg

exchangeRates :: (MonadHttp m) => Config -> m Object
exchangeRates cfg = responseBody <$> req
  GET
  (https baseUrl /: path)
  NoReqBody
  jsonResponse
  (header "Accept" "application/json")
 where
  path    = "/v1/rates"
  baseUrl = configUrl cfg

charges :: (MonadHttp m) => Config -> m (ResponseData [Charge])
charges cfg = responseBody <$> req
  GET
  (https baseUrl /: path)
  NoReqBody
  jsonResponse
  (header "Authorization" token <> header "Accept" "application/json")
  where
    path = "/v1/charges"
    baseUrl = configUrl cfg
    token = configToken cfg

charge :: (MonadHttp m) => Config -> Text -> m (ResponseData Charge)
charge cfg cid = responseBody <$> req
  GET
  (https baseUrl /: path /: cid)
  NoReqBody
  jsonResponse
  (header "Authorization" token <> header "Accept" "application/json")
  where
    path = "/v1/charge/"
    baseUrl = configUrl cfg
    token = configToken cfg

createCharge :: (MonadHttp m) => Config -> ChargeRequest -> m (ResponseData ChargeResponse)
createCharge cfg creq = responseBody <$> req
  POST
  (https baseUrl /: path)
  (ReqBodyJson creq)
  jsonResponse
  (header "Authorization" token <> header "Accept" "application/json")
  where
    path = "/v1/charges"
    baseUrl = configUrl cfg
    token = configToken cfg
