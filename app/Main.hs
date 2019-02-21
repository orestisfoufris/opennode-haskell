{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class (liftIO)
import           Data.Default.Class     (def)
import           Network.HTTP.Req

import           OpenNode.Api
import           OpenNode.Config

-- TODO :: use UUID library for UUID's

main :: IO ()
main = runReq def $ do
  r <- withdrawal prodConfig ""
  liftIO $ print r
  ws <- withdrawals prodConfig
  liftIO $ print ws
  currencies <- exchangeRates prodConfig
  liftIO $ print currencies
  charges <- charges prodConfig
  liftIO $ print charges
  ch <- charge prodConfig ""
  liftIO $ print ch
