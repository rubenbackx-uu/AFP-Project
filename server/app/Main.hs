{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Properties
import Repository
import Control.Monad
import Data.Maybe
import Entity.User
import Entity.Artist
import Entity.Tabs
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import Database.MySQL.Simple
import Control.Monad.IO.Class (MonadIO(liftIO))
import GHC.Generics
import Servant.Auth (Auth)
import Servant.Auth.Server as SAS
import Servant.Auth as SA
import Control.Exception.Base (throwIO)

type UserAPI = "api" :> "users" :> Get '[JSON] [User] :<|> "api" :> "user" :> Capture "username" String :> Get '[JSON] User
type UserAPIServer = Auth '[SA.JWT, SA.BasicAuth] User :> UserAPI

users :: Connection -> IO [User]
users conn = findAll conn usersTable

singleUser :: Connection -> String -> IO User
singleUser conn username = do
  users <- query conn "SELECT * FROM users WHERE username = ?" (Only username)
  case users of
    [user] -> return user
    _ -> throwIO $ userError "User not found"
-- TODO: Error handling

server :: Connection -> Server UserAPIServer
server conn (Authenticated user) = handleUsers :<|> handleUser
    where
        handleUsers = liftIO $ users conn
        handleUser requestedUsername = if requestedUsername == username user then liftIO (singleUser conn requestedUsername) else throwError err401
server _ _ = throwAll err401

authCheck :: Connection -> BasicAuthData -> IO (AuthResult User)
authCheck conn (BasicAuthData username password) = do
  users <- query conn "SELECT * FROM users WHERE username = ? AND password = ?" (username, password)
  case users of
    [user] -> return (Authenticated user)
    _ -> return SAS.BadPassword

corsConfig :: Middleware
corsConfig = cors $ const $ Just simpleCorsResourcePolicy {
    corsRequestHeaders = "Authorization" : corsRequestHeaders simpleCorsResourcePolicy
}

app :: Connection -> IO Application
app conn = do
    myKey <- generateKey
    let jwtCfg = defaultJWTSettings myKey
        authCfg = authCheck conn
        cfg = jwtCfg :. defaultCookieSettings :. authCfg :. EmptyContext
        api = Proxy :: Proxy UserAPIServer
    return $ corsConfig $ serveWithContext api cfg (server conn)

main :: IO ()
main =
    do properties <- readPropertiesFile "application.properties"

       conn <- connect defaultConnectInfo { connectUser = "afp", connectPassword = "pass", connectDatabase = "afp" }

       let port = fromMaybe 8300 $ getIntegerProperty properties "port"
       let settings = setPort port $ setBeforeMainLoop (putStrLn ("listening on port " ++ show port)) defaultSettings

       let dropTables = getBooleanProperty properties "database.drop-tables"

       when dropTables $ dropTable conn usersTable
       when dropTables $ dropTable conn tabsTable
       when dropTables $ dropTable conn artistsTable

       let createTables = getBooleanProperty properties "database.create-tables"
       when createTables $ createTable conn usersTable
       when createTables $ createTable conn artistsTable
       when createTables $ createTable conn tabsTable

       let clearTables = getBooleanProperty properties "database.clear-tables"
       when clearTables $ deleteAll conn usersTable
       when clearTables $ deleteAll conn tabsTable
       when clearTables $ deleteAll conn artistsTable

       let profile = fromMaybe "default" $ getProperty properties "profile"
       when (profile == "dev") $ initDevData conn

       runSettings settings =<< app conn

initDevData :: Connection -> IO ()
initDevData conn =
    do _ <- saveAll conn usersTable [User 0 "user1" "user1", User 0 "user2" "user2"]
       return ()
