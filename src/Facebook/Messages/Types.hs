{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics

data Message =
  Message { mUser :: T.Text
          , mTimestamp :: T.Text
          , mMessage :: T.Text
          } deriving (Show, Eq, Generic)

instance ToJSON Message where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Message
