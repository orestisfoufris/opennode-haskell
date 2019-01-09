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
    r <- getWithdrawal "dev-api.opennode.co" "/v1/withdrawal/" "123" "1fbcdbb7-ad6d-4487-ba07-974542fdb497"
    print (responseBody r :: Value)

getWithdrawal :: (MonadHttp m, FromJSON a) => T.Text -> String -> String -> B.ByteString -> m (JsonResponse a)
getWithdrawal baseUrl path withId token =
  req
    GET
    (https baseUrl /: T.pack ((++) path withId))
    NoReqBody
    jsonResponse
    (header "Authorization" token <> header "Accept" "application/json")
