module Artist (Artist(..)) where

import Database.MySQL.Simple 

data Artist = Artist {id :: Int, name :: String}