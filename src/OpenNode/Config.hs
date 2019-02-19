{-# LANGUAGE OverloadedStrings #-}

module OpenNode.Config
  ( prodConfig
  , devConfig
  , Config (..)
  )
where

import           Data.ByteString.Char8
import           Data.Text

data Config = Config
    { configToken :: !ByteString
    , configUrl   :: !Text
    }

-- TODO: tokens should come from ENVS

prodToken :: ByteString
prodToken = "insert prod token here"

prodUrl :: Text
prodUrl = "api.opennode.co"

devToken :: ByteString
devToken = "insert dev token here"

devUrl :: Text
devUrl = "dev-api.opennode.co"

prodConfig = Config prodToken prodUrl
devConfig = Config prodToken prodUrl
