{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Entity.User where

import Table
import Repository
import Control.Monad
import Data.Aeson ( ToJSON, FromJSON )
import Data.String
import GHC.Generics
import Database.MySQL.Simple 
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.Result 
import Servant.Auth.JWT
import Servant.Auth.Server as SAS

data User = User { userId :: Int, username :: String, password :: String } deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User
instance ToJWT User
instance FromJWT User

instance QueryResults User where
    convertResults [f0, f1, f2] [v0, v1, v2] = User id name password
        where id = convert f0 v0
              name = convert f1 v1
              password = convert f2 v2
    convertResults fs vs = convertError fs vs 2 

usersTable :: Table User
usersTable = Table {
    tableName = "users",
    idCol = "id",
    definition = "( id BIGINT PRIMARY KEY NOT NULL AUTO_INCREMENT, username VARCHAR(255) NOT NULL, password VARCHAR(255) NOT NULL )"
}

instance Repository User where
    save conn table user = void $ execute conn (fromString ("INSERT INTO " ++ tableName table ++ " (username, password) VALUES (?, ?)")) (username user, password user)
    delete conn table user = deleteById conn table (userId user)

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult User)

instance FromBasicAuthData User where
  fromBasicAuthData :: BasicAuthData -> BasicAuthCfg -> IO (AuthResult User)
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData