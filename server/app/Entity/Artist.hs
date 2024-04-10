{-# LANGUAGE DeriveGeneric #-}

module Entity.Artist(Artist(..)) where

import Table
import Repository
import Control.Monad
import Data.Aeson ( ToJSON )
import Data.String
import GHC.Generics
import Database.MySQL.Simple 
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.Result 

data Artist = Artist { artistId :: Int, artistName :: String }
    deriving (Show, Eq, Generic)

instance ToJSON Artist

instance QueryResults Artist where
    convertResults [fa, fb] [va, vb] = Artist id name
        where id = convert fa va
              name = convert fb vb
    convertResults fs vs = convertError fs vs 2 

artistsTable :: Table Artist
artistsTable = Table {
    tableName = "artists",
    idCol = "id",
    definition = "( id BIGINT PRIMARY KEY NOT NULL AUTO_INCREMENT, name VARCHAR(255) NOT NULL )"
}

instance Repository Artist where
    save conn table artist = void $ execute conn (fromString ("INSERT INTO " ++ tableName table ++ " (name) VALUES (?)")) (Only (artistName artist))
    delete conn table artist = deleteById conn table (artistId artist)