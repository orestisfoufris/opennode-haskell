{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception      (throwIO)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Char8  as B
import           Data.Maybe             (fromJust)
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import           GHC.Generics
import           Network.HTTP.Req

instance MonadHttp IO where
  handleHttpException = throwIO

data Resp = Resp
  { success :: String
  , message :: String
  } deriving (Show, Generic, ToJSON, FromJSON)

main :: IO ()
main =
   do
    r <- getWithdrawal "dev-api.opennode.co" "/v1/withdrawal/" "123" "c4fb8b63-bf77-4116-aad3-9195b5d11c9f"
    print (responseBody r :: Value)

getWithdrawal :: (MonadHttp m, FromJSON a) => T.Text -> T.Text -> T.Text -> B.ByteString -> m (JsonResponse a)
getWithdrawal baseUrl path withId token =
  req
    GET
    (https baseUrl /: (<>) path withId)
    NoReqBody
    jsonResponse
    (header "Authorization" token <> header "Accept" "application/json")
