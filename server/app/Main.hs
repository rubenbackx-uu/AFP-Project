{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Properties
import Repository
import Control.Monad
import Data.Maybe
import Data.List (intercalate)
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
import Entity.Artist (artistsTable)

type LoginAPI = "login" :> Post '[JSON] User
type LoginAPIServer = Auth '[SA.JWT, SA.BasicAuth] User :> LoginAPI

serveLogin :: AuthResult User -> Handler User
serveLogin (Authenticated user) = return user
serveLogin _ = throwError err401

type AllTabsAPI = "tabs" :> Get '[JSON] [TabSummaryDTO]
type AllTabsAPIServer = AllTabsAPI

allTabs :: Connection -> IO [TabSummaryDTO]
allTabs conn = 
    do tabs <- findAll conn tabsTable
       concat <$> (traverse maybeToList) <$> traverse tabToDTO tabs
    where tabToDTO :: Tabs -> IO (Maybe TabSummaryDTO)
          tabToDTO tab = do mbArtist <- findById conn artistsTable (tabArtistId tab)
                            return $ (toTabSummaryDTO tab) <$> mbArtist

serveAllTabs :: Connection -> Handler [TabSummaryDTO]
serveAllTabs conn = liftIO $ allTabs conn

type TabByIdAPI = "tab" :> Capture "tabId" Int :> Get '[JSON] TabDTO
type TabByIdAPIServer = TabByIdAPI

tabById :: Connection -> Int -> IO (Maybe TabDTO)
tabById conn id = 
    do mbTab <- findById conn tabsTable id
       case mbTab of
           Just tab -> 
               do mbArtist <- findById conn artistsTable (tabArtistId tab)
                  return $ (\artist -> toTabDTO tab artist) <$> mbArtist
           Nothing -> return Nothing

serveTabById :: Connection -> Int -> Handler TabDTO
serveTabById conn tabId = 
    do mbTab <- liftIO $ tabById conn tabId
       case mbTab of
           Just tab -> return tab
           Nothing -> throwError err404

type API = LoginAPIServer :<|> AllTabsAPIServer :<|> TabByIdAPIServer

server :: Connection -> Server API
server conn = serveLogin :<|> (serveAllTabs conn) :<|> (serveTabById conn)

authCheck :: Connection -> BasicAuthData -> IO (AuthResult User)
authCheck conn (BasicAuthData username password) = do
  users <- query conn "SELECT * FROM users WHERE username = ? AND password = ?" (username, password)
  case users of
    [user] -> return (Authenticated user)
    _ -> return SAS.BadPassword

corsConfig :: Middleware
corsConfig = cors $ const $ Just simpleCorsResourcePolicy {
    corsRequestHeaders = "Authorization" : "Cookie" : corsRequestHeaders simpleCorsResourcePolicy
}

app :: Connection -> IO Application
app conn = do
    myKey <- generateKey
    let jwtCfg = defaultJWTSettings myKey
        authCfg = authCheck conn
        cfg = jwtCfg :. defaultCookieSettings :. authCfg :. EmptyContext
        api = Proxy :: Proxy API
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
       _ <- save conn artistsTable $ Artist 0 "Eagles"
       _ <- save conn artistsTable $ Artist 0 "Vance Joy"
       [artist1, artist2] <- findAll conn artistsTable
       let riptide = (intercalate "\n" [
                        "Am              G                C",
                        "I was scared of dentists and the dark",
                        "Am              G",
                        "I was scared of pretty girls and starting conversations",
                        "    Am        G                C",
                        "Oh, all my friends are turning green",
                        "           Am           G                 C",
                        "You're the magician's assistant  in their dreams",
                        "","",
                        "Am   G    C",
                        "Ooh, ooh, ooh",
                        "Am   G             C",
                        "Ooh, ooh, and they come unstuck",
                        "Am    G                   C",
                        "Lady, running down to the riptide, taken away",
                        "       Am         G               C",
                        "to the dark side, I wanna be your left hand man",
                        "  Am       G                        C",
                        "I love you when you're singing that song, and I got a lump",
                        "      Am             G                     C",
                        "in my throat, 'cause you're gonna sing the words wrong"
                        ])
       _ <- save conn tabsTable $ Tabs 0 "Riptide" (artistId artist2) Chords riptide 4
       let hotelCalifornia = (intercalate "\n" [
                        "Am                        E7",
                        "On a dark desert highway, cool wind in my hair",
                        "G                     D",
                        "Warm smell of colitas rising up through the air",
                        "F                         C",
                        "Up ahead in the distance, I saw a shimering light",
                        "Dm",
                        "My head grew heavy and my sight grew dim",
                        "E7",
                        "I had to stop for the night",
                        "","",
                        "Am                              E7",
                        "There she stood in the doorway, I heard the mission bell",
                        "G",
                        "And I was thinking to myself",
                        "              D",
                        "This could be heaven or this could be hell",
                        "F                         C",
                        "Then she lit up a candle, and she showed me the way",
                        "Dm",
                        "There were voices down the corridor",
                        "E7",
                        "I thought I heard them say..."
                        ])
       _ <- save conn tabsTable $ Tabs 0 "Hotel California" (artistId artist1) Chords hotelCalifornia 5
       return ()
