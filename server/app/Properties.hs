{-# LANGUAGE OverloadedStrings #-}

module Properties where

import Data.Maybe ( fromJust, isJust )
import Data.Foldable (find)
import Data.String ( IsString(fromString) )
import Data.Text ( splitOn, strip, unpack )
import System.IO

data Property = Property { propKey :: String, propVal :: String }
    deriving (Show, Eq)

readPropertiesFile :: String -> IO [Property]
readPropertiesFile fileName = 
    do handle <- openFile fileName ReadMode
       content <- hGetContents handle
       return $ fromJust <$> Prelude.filter isJust (toProperty <$> filter (\line -> '=' `elem` line) (lines content))

getProperty :: [Property] -> String -> Maybe String
getProperty props key = propVal <$> find (\p -> propKey p == key) props

getBooleanProperty :: [Property] -> String -> Bool
getBooleanProperty props key = getProperty props key == Just "true"

getIntegerProperty :: [Property] -> String -> Maybe Int
getIntegerProperty props key = read <$> getProperty props key

toProperty :: String -> Maybe Property
toProperty s = let parts = splitOn "=" (fromString s) in
    if length parts == 2 then Just $ Property (unpack (strip (parts !! 0))) (unpack (strip (parts !! 1)))
    else Nothing