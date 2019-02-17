{-# LANGUAGE OverloadedStrings #-}

module OpenNode.Api
  ( withdrawal
  , withdrawals
  , exchangeRates
  , charges
  )
where

import           Data.Aeson         (Object)
import           Data.Default.Class (def)
import           Data.Monoid        ((<>))
import           Data.Text          (Text)
import           Network.HTTP.Req
import           OpenNode.Config
import           OpenNode.Data

withdrawal :: (MonadHttp m) => Config -> Text -> m (ResponseData Withdrawal)
withdrawal cfg uuid = responseBody <$> req
  GET
  (https baseUrl /: path /: uuid)
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
