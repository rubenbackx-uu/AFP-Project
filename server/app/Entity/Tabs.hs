{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Entity.Tabs where

import Table
import Repository
import Control.Monad
import Data.Aeson hiding (Result)
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

data TabArtistDTO = TabArtistDTO { tabArtistDtoId :: Int, tabArtistDtoName :: String }
data TabDTO = TabDTO { tabDtoId :: Int, tabDtoTitle :: String, tabDtoArtist :: TabArtistDTO, tabDtoType :: TabType, tabDtoContent :: TabContent }

instance ToJSON TabArtistDTO where 
    toJSON dto = object [ "id" .= tabArtistDtoId dto, "name" .= tabArtistDtoName dto ]
instance ToJSON TabDTO where 
    toJSON dto = object [ "id" .= tabDtoId dto, "title" .= tabDtoTitle dto, "artist" .= tabDtoArtist dto, "type" .= tabDtoType dto, "content" .= tabDtoContent dto ]

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



newtype TabContent = TabContent [ChordTabLine] deriving Show 
instance ToJSON TabContent where
    toJSON (TabContent lines) = object [ "lines" .= lines ]

data ChordTabLine = ChordTabLine String [IndexedChord] deriving Show
instance ToJSON ChordTabLine where 
    toJSON (ChordTabLine text chords) = object [ "text" .= text, "chords" .= chords ]

data IndexedChord = IndexedChord Int Chord deriving Show
instance ToJSON IndexedChord where 
    toJSON (IndexedChord pos chord) = object [ "position" .= pos, "chord" .= chord ]

data Chord = Chord Entity.Tabs.Key Mode  -- for instance, in a F#m7 chord F# is the Key, and m7 the Mode
instance Show Chord where
    show (Chord key mode) = show key ++ mode
instance ToJSON Chord where 
    toJSON chord = toJSON (show chord)
type Mode = String

data Key = A | ASharp | B | C | CSharp | D | DSharp | E | F | FSharp | G | GSharp | InvalidKey deriving (Eq)
instance Show Entity.Tabs.Key where 
    show A = "A"
    show ASharp = "A#"
    show B = "B"
    show C = "C#"
    show D = "D"
    show DSharp = "D#"
    show E = "E"
    show F = "F"
    show FSharp = "F#"
    show G = "G"
    show GSharp = "G#"
    show InvalidKey = "?"
instance ToJSON Entity.Tabs.Key where
    toJSON key = toJSON (show key)

-- Parsing
-- makeTab :: Int -> String -> String -> String -> Maybe Tab
-- makeTab i t a c = case run parseChordTab c of
--         Nothing      -> Nothing
--         Just con -> Just (Tab i t a con)

-- parseChordTab :: Parser Char TabContent
-- parseChordTab = ChordTab <$> many parseChordTabLine

-- parseChordTabLine :: Parser Char ChordTabLine
-- parseChordTabLine = parseChordLine <|> parsePlainTextLine

-- parseChordLine :: Parser Char ChordTabLine
-- parseChordLine = ChordLine <$> ((\i cs _ -> makeIndexedChords i cs) <$> parseWhiteSpaces <*> many parseChordAndSpace <*> parseCrlf)

-- makeIndexedChords :: Int -> [(Chord, Int)] -> [IndexedChord]
-- makeIndexedChords _ [] = []
-- makeIndexedChords i ((c, s):xs) = IndexedChord i c : makeIndexedChords (i + chordLength c + s) xs

-- chordLength :: Chord -> Int
-- chordLength (Chord k m) = keyLength k + length m

-- keyLength :: Key -> Int
-- keyLength c = if c `elem` [A, B, C, D, E, F, G] then 1 else 2

-- parseChordAndSpace :: Parser Char (Chord, Int)
-- parseChordAndSpace = (,) <$> parseChord <*> parseWhiteSpaces

-- parsePlainTextLine :: Parser Char ChordTabLine
-- parsePlainTextLine = (\t _ -> PlainTextLine t) <$> parseText <*> parseCrlf

-- parseChord :: Parser Char Chord
-- parseChord = Chord <$> parseKey <*> parseSignature

-- parseSignature :: Parser Char String
-- parseSignature = concat <$> many (token "maj" <|> token "min" <|> token "m" <|> token "sus" <|> token "dim" <|> token "/" <|> token "add"
--         <|> token "2" <|> token "4" <|> token "5" <|> token "6" <|> token "7" <|> token "9" <|> token "11" <|> token "13" <|>
--          token "A" <|> token "B" <|> token "C" <|> token "D" <|> token "E" <|> token "F" <|> token "G")

-- char :: Char -> Parser Char Char
-- char c = satisfy (==c)

-- parseKey :: Parser Char Key
-- parseKey = mkKey <$> satisfy isKey <*> (char '#' <|> char 'b' <|> succeed ' ')

-- isKey :: Char -> Bool
-- isKey c = c >= 'A' && c <= 'G'

-- mkKey:: Char -> Char -> Key
-- mkKey 'A' '#' = ASharp
-- mkKey 'C' '#' = CSharp
-- mkKey 'D' '#' = DSharp
-- mkKey 'F' '#' = FSharp
-- mkKey 'G' '#' = GSharp
-- mkKey 'B' 'b' = ASharp
-- mkKey 'D' 'b' = CSharp
-- mkKey 'E' 'b' = DSharp
-- mkKey 'G' 'b' = FSharp
-- mkKey 'A' 'b' = GSharp
-- mkKey 'A' ' ' = A
-- mkKey 'B' ' ' = B
-- mkKey 'C' ' ' = C
-- mkKey 'D' ' ' = D
-- mkKey 'E' ' ' = E
-- mkKey 'F' ' ' = F
-- mkKey 'G' ' ' = G
-- mkKey _ _ = InvalidKey

-- parseWhiteSpaces :: Parser Char Int
-- parseWhiteSpaces = (length <$> many (char ' ')) <|> succeed 0

-- parseCrlf :: Parser Char [Char]
-- parseCrlf = token "\n" <|> token "\r\n"

-- parseText :: Parser Char [Char]
-- parseText = many (satisfy (\c -> c /= '\n' && c /= '\r'))

-- run :: Parser a b -> [a] -> Maybe b
-- run p xs | null (parse p xs) = Nothing
--          | otherwise = Just (fst (head (parse p xs)))