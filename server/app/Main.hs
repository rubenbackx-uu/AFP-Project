{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import Database.MySQL.Simple 
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.Result 
import Control.Monad.IO.Class (MonadIO(liftIO))

type UserAPI = "users" :> Get '[JSON] [User]

data User = User { id :: Int, name :: String } deriving (Eq, Show, Generic)

instance ToJSON User
instance QueryResults User where
    convertResults [fa, fb] [va, vb] = User id name
        where id = convert fa va
              name = convert fb vb
    convertResults fs vs = convertError fs vs 2 

users :: Connection -> IO [User]
users conn = query_ conn "SELECT * FROM users" --[User 1 "User 1", User 2 "User 2"]

server :: Connection -> Server UserAPI
server conn = getUsers
    where getUsers :: Handler [User]
          getUsers = liftIO $ users conn

userAPI :: Proxy UserAPI
userAPI = Proxy

app :: Connection -> Application
app conn = simpleCors $ serve userAPI (server conn)

main :: IO ()
main = 
    do conn <- connect defaultConnectInfo { connectUser = "afp", connectPassword = "pass", connectDatabase = "afp" } 
       _ <- execute_ conn "CREATE TABLE IF NOT EXISTS users ( id BIGINT PRIMARY KEY NOT NULL AUTO_INCREMENT, name VARCHAR(255) NOT NULL )"
       _ <- execute_ conn "DELETE FROM users"
       _ <- execute_ conn "INSERT INTO users (name) VALUES ('User 1'), ('User 2')"
       _ <- execute_ conn "CREATE TABLE IF NOT EXISTS artist ( id BIGINT PRIMARY KEY NOT NULL AUTO_INCREMENT, name VARCHAR (255) NOT NULL )"
       run 8300 (app conn)


