{-# LANGUAGE DeriveGeneric #-}
module User (User(..)) where

import GHC.Generics
import Data.Aeson
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.Result
import Servant.Auth.JWT


data User = User { id :: Int, name :: String } deriving (Eq, Show, Generic)

instance ToJSON User

instance QueryResults User where
    convertResults [fa, fb] [va, vb] = User id name
        where id = convert fa va
              name = convert fb vb
    convertResults fs vs = convertError fs vs 2 

instance ToJWT User