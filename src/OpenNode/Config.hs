{-# LANGUAGE OverloadedStrings #-}

module OpenNode.Config
  ( Config (..)
  )
where

import           Data.ByteString.Char8
import           Data.Text

data Config = Config
    { configToken :: !ByteString
    , configUrl   :: !Text
    } deriving ( Show )
