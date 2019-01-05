{-# LANGUAGE DuplicateRecordFields #-}

module Data where

data WithdrawalsReq a = WithdrawalsReq
  { wtype :: WithdrawalType
  , amount :: a
  , address :: String
  } deriving (Show, Eq)

data WithdrawalsResp a = WithdrawalsResp
  { id :: String
  , wtype :: WithdrawalType
  , amount :: a
  , reference :: String
  , processedAt :: String
  , address :: String
  , fee :: a
  } deriving (Show, Eq)

data WithdrawalType
  = LN
  | CHAIN
  deriving (Eq)

instance Show WithdrawalType where
  show LN = "ln"
  show CHAIN = "chain"