{-# LANGUAGE OverloadedStrings, TypeFamilies, DataKinds,
  DeriveGeneric, TypeOperators #-}
module User (User(..)) where

import GHC.Generics
import Data.Aeson
import Database.MySQL.Simple 
import Servant.Auth.JWT
import Servant.Auth.Server as SAS
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.Result (convert)


data User = User { id :: Int, name :: String, password :: String } deriving (Eq, Show, Generic)

instance ToJSON User

instance QueryResults User where
    convertResults [fa, fb, fc] [va, vb, vc] = User id name password
        where id = convert fa va
              name = convert fb vb
              password = convert fc vc
    convertResults fs vs = convertError fs vs 2 

instance ToJWT User

instance FromJSON User

instance FromJWT User

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult User)

instance FromBasicAuthData User where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData
