{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class (liftIO)
import           Data.Default.Class     (def)
import           Network.HTTP.Req

import           OpenNode.Api
import           OpenNode.Config
import           Data.Aeson (Object)
-- TODO :: use UUID library for UUID's

main :: IO ()
main = runReq def $ do
  r <- withdrawal prodConfig "9741f448-500b-431f-91ac-b40028dd5e5b"
  liftIO $ print r
  ws <- withdrawals prodConfig
  liftIO $ print ws
  currencies <- exchangeRates prodConfig
  liftIO $ print (currencies :: Object)