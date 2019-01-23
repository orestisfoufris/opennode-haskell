{-# LANGUAGE OverloadedStrings #-}

module OpenNode.Config
  ( prodConfig
  , devConfig
  , Config (..)
  )
where

import Data.ByteString.Char8
import Data.Text

data Config = Config
    { configToken :: !ByteString
    , configUrl   :: !Text
    }

prodToken :: ByteString
prodToken = "81822829-ee7f-4024-8772-b1bfbb4a7123"

prodUrl :: Text
prodUrl = "api.opennode.co"

devToken :: ByteString
devToken = "1f021a7b-6011-473e-b857-58d77d04b39c"

devUrl :: Text
devUrl = "dev-api.opennode.co"

prodConfig = Config prodToken prodUrl
devConfig = Config prodToken prodUrl
