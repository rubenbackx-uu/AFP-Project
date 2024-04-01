{-# LANGUAGE OverloadedStrings #-}

module Repository where

import Table
import Data.String
import Data.Foldable (traverse_)
import Control.Monad
import Database.MySQL.Simple 
import Database.MySQL.Simple.QueryResults

class QueryResults a => Repository a where
    findById :: Param id => Connection -> Table a -> id -> IO (Maybe a)
    findById conn table id =
        do res <- query conn (fromString ("SELECT * FROM " ++ tableName table ++ " WHERE " ++ idCol table ++ " = ?")) (Only id)
           return $ if length res == 1 then Just (head res) else Nothing

    findByIds :: Param id => Connection -> Table a -> [id] -> IO [a]
    findByIds conn table ids = query conn (fromString ("SELECT * FROM " ++ tableName table ++ " WHERE " ++ idCol table ++ " IN ?")) (Only (In ids))

    findAll :: Connection -> Table a -> IO [a]
    findAll conn table = query_ conn (fromString ("SELECT * FROM " ++ tableName table))
    
    save :: Connection -> Table a -> a -> IO ()
    saveAll :: Connection -> Table a -> [a] -> IO ()
    saveAll conn table = traverse_ (save conn table)

    delete :: Connection -> Table a -> a -> IO ()
    deleteById :: Param id => Connection -> Table a -> id -> IO ()
    deleteById conn table id = void $ execute conn (fromString ("DELETE FROM " ++ tableName table ++ " WHERE " ++ idCol table ++ " = ?")) (Only id)
    deleteAll :: Connection -> Table a -> IO ()
    deleteAll conn table = void $ execute_ conn (fromString ("DELETE FROM " ++ tableName table))

    createTable :: Connection -> Table a -> IO ()
    createTable conn table = void $ execute_ conn (fromString ("CREATE TABLE IF NOT EXISTS " ++ tableName table ++ " " ++ definition table))
    dropTable :: Connection -> Table a -> IO ()
    dropTable conn table = void $ execute_ conn (fromString ("DROP TABLE IF EXISTS " ++ tableName table))

