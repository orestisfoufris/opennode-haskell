{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class (liftIO)
import           Data.Default.Class     (def)
import           Data.Text
import           Network.HTTP.Req
import           OpenNode.Api
import           OpenNode.Config
import           OpenNode.Data
import qualified Options.Applicative    as O

-- TODO :: use UUID library for UUID's

main :: IO ()
main =
  run =<< O.execParser parser
  where
    parser = O.info (O.helper <*> opts)
      (  O.fullDesc
      <> O.progDesc "OpenNode Client"
      <> O.header "opennode-haskell"
      )
    opts = Config
      <$> O.strOption
        (  O.long "token"
        <> O.metavar "TOKEN"
        <> O.help "Authentication Token"
        )
      <*> O.strOption
        (  O.long "url"
        <> O.value "dev-api.opennode.co" <> O.showDefault
        <> O.metavar "URL"
        <> O.help "Url to connect"
        )

run :: Config -> IO ()
run config = do
  runReq def $ do
    liftIO $ print config
    r <- withdrawal config ""
    liftIO $ print r
  -- ws <- withdrawals devConfig
  -- liftIO $ print ws
  -- currencies <- exchangeRates prodConfig
  -- liftIO $ print currencies
  -- charges <- charges prodConfig
  -- liftIO $ print charges
  -- ch <- charge prodConfig ""
  -- liftIO $ print ch
  --  do
  --   cc <- createCharge devConfig chargeReq
  --   liftIO $ print cc
  -- where
  --   chargeReq =
  --     ChargeRequest
  --       "some description"
  --       0.01
  --       "GBP"
  --       Nothing
  --       "testemail@test.com"
  --       Nothing
  --       Nothing
  --       Nothing
  --       (Just False)
