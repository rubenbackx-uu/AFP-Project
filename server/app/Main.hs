{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import Database.MySQL.Simple 
import Control.Monad.IO.Class (MonadIO(liftIO))
import User (User(..))
import GHC.Generics
import Servant.Auth (Auth)
import Servant.Auth.Server as SAS
import Servant.Auth as SA

type UserAPI = "users" :> Get '[JSON] [User]
type UserAPIServer = Auth '[SA.JWT, SA.BasicAuth] User :> UserAPI


users :: Connection -> IO [User]
users conn = query_ conn "SELECT * FROM users" --[User 1 "User 1", User 2 "User 2"]

server :: Connection -> Server UserAPIServer
server conn (Authenticated user) = liftIO $ users conn
server _ _ = throwError err401

userAPI :: Proxy UserAPI
userAPI = Proxy

authCheck :: Connection -> BasicAuthData -> IO (AuthResult User)
authCheck conn (BasicAuthData username password) = do
  users <- query conn "SELECT * FROM users WHERE name = ? AND password = ?" (username, password)
  case users of
    [user] -> return (Authenticated user)
    _ -> return SAS.BadPassword

app :: Connection -> IO Application
app conn = do
    myKey <- generateKey
    let jwtCfg = defaultJWTSettings myKey
        authCfg = authCheck conn
        cfg = jwtCfg :. defaultCookieSettings :. authCfg :. EmptyContext
        api = Proxy :: Proxy UserAPIServer
    pure $ serveWithContext api cfg (server conn)

main :: IO ()
main = do
    conn <- connect defaultConnectInfo { connectUser = "afp", connectPassword = "pass", connectDatabase = "afp" } 
    let settings = setPort 8300 $ setBeforeMainLoop (putStrLn ("listening on port " ++ show 8300)) $ defaultSettings
    _ <- execute_ conn "CREATE TABLE IF NOT EXISTS users ( id BIGINT PRIMARY KEY NOT NULL AUTO_INCREMENT, name VARCHAR(255) NOT NULL, password VARCHAR(255) NOT NULL)"
    _ <- execute_ conn "DELETE FROM users"
    _ <- execute_ conn "INSERT INTO users (name, password) VALUES ('user1', 'pw1'), ('user2', 'pw2')"
    runSettings settings =<< app conn


