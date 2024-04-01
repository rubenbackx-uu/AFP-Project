{-# LANGUAGE DeriveGeneric #-}

module Entity.Tabs where

import Table
import Repository
import Control.Monad
import Data.Aeson ( ToJSON, FromJSON )
import Data.String
import Data.ByteString
import GHC.Generics
import Database.MySQL.Base.Types
import Database.MySQL.Simple 
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.Param 
import Database.MySQL.Simple.Result 

data TabType = Chords | Tab
    deriving (Show, Eq, Generic)

instance ToJSON TabType

instance ToField TabType where
    toField t = fromString $ show t

instance Param TabType where

instance Result TabType where
    convert f v = case convert f v of
        "Chords" -> Chords
        "Tab" -> Tab
        s -> error $ "Invalid tab type: " ++ s

data Tabs = Tabs { tabId :: Int, tabTitle :: String, tabArtistId :: Int, tabType :: TabType, tabContent :: String }
    deriving (Show, Eq, Generic)

instance ToJSON Tabs

instance QueryResults Tabs where
    convertResults [f0, f1, f2, f3, f4] [v0, v1, v2, v3, v4] = Tabs id title artistId tabType content
        where id = convert f0 v0
              title = convert f1 v1
              artistId = convert f2 v2
              tabType = convert f3 v3
              content = convert f4 v4
    convertResults fs vs = convertError fs vs 2 

tabsTable :: Table Tabs
tabsTable = Table {
    tableName = "tabs",
    idCol = "id",
    definition = "( id BIGINT PRIMARY KEY NOT NULL AUTO_INCREMENT, title VARCHAR(255) NOT NULL, artist_id BIGINT NOT NULL, type ENUM('Chords', 'Tabs') NOT NULL, tab_content TEXT NOT NULL, CONSTRAINT `fk_tab_art` FOREIGN KEY (artist_id) REFERENCES artists (id) ON DELETE CASCADE ON UPDATE RESTRICT )"
}

instance Repository Tabs where
    save conn table tab = void $ execute conn (fromString ("INSERT INTO " ++ tableName table ++ " (title, artist_id, type, tab_content) VALUES (?)")) (tabTitle tab, tabArtistId tab, tabType tab, tabContent tab)
    delete conn table tab = deleteById conn table (tabId tab)