{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

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
main = run =<< O.execParser parser
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
run config =
  runReq defaultHttpConfig $ do
    r <- accountBalance config
    liftIO $ print r
    -- r <- exchangeRates config
    -- liftIO $ print r
    -- r <- supportedCurrencies config
    -- liftIO $ print r
    -- let cr = ChargeRequest "" 10.0 "EUR" Nothing "this@this.com" Nothing Nothing Nothing Nothing
    -- r <- createCharge config cr
    -- liftIO $ print r
    -- let faucet = "2NGZrVvZG92qGYqzTLjCAewvPZ7JE8S8VxE"
    -- let wr = WithdrawalRequest Chain 10000 faucet ""
    -- r <- createWithdrawal config wr
    -- liftIO $ print r
