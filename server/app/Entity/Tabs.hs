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
import ParseLib.Abstract

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



data TabContent = ChordTabContent [ChordTabLine] | TabTabContent [TabElement] deriving Show 
instance ToJSON TabContent where
    toJSON (ChordTabContent lines) = object [ "lines" .= lines ]
    toJSON (TabTabContent elements) = object [ "elements" .= elements]

data ChordTabLine = ChordTabLine String [IndexedChord] deriving Show
instance ToJSON ChordTabLine where 
    toJSON (ChordTabLine text chords) = object [ "text" .= text, "chords" .= chords ]

data IndexedChord = IndexedChord Int Chord deriving Show
instance ToJSON IndexedChord where 
    toJSON (IndexedChord pos chord) = object [ "position" .= pos, "chord" .= chord ]

data Chord = SimpleChord Entity.Tabs.Key Mode | ChordWithBass Entity.Tabs.Key Mode Entity.Tabs.Key  -- for instance, in a F#m7 chord F# is the Key, and m7 the Mode
instance Show Chord where
    show (SimpleChord k m)     = show k ++ m
    show (ChordWithBass k m b) = show k ++ m ++ "/" ++ show b
instance ToJSON Chord where 
    toJSON chord = toJSON (show chord)
type Mode = String

data Key = A | ASharp | B | C | CSharp | D | DSharp | E | F | FSharp | G | GSharp | InvalidKey deriving (Eq)
instance Show Entity.Tabs.Key where 
    show A = "A"
    show ASharp = "A#"
    show B = "B"
    show C = "C"
    show CSharp = "C#"
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

--Parsing
getType :: TabContent -> TabType
getType (ChordTabContent _) = Chords
getType (TabTabContent _)   = Tab

makeTabContent :: String -> Maybe TabContent
makeTabContent s = run parseTab (s ++ "\n##EOF##") >>= validate

parseTab :: Parser Char TabContent
parseTab = ((\x _ -> TabTabContent x) <$> many parseTabElement <*> parseEndOfFile) <|> ((\x _ ->ChordTabContent x) <$> many parseChordTabLine <*> parseEndOfFile)

parseChordTabLine :: Parser Char ChordTabLine
parseChordTabLine = (flip ChordTabLine <$> parseChordLine <*> parsePlainTextLine) <|> (flip ChordTabLine <$> parseChordLine <*> succeed "") <|> (flip ChordTabLine <$> succeed [] <*> parsePlainTextLine)

parseChordLine :: Parser Char [IndexedChord]
parseChordLine = (\i cs _ -> makeIndexedChords i cs) <$> parseWhiteSpaces <*> many parseChordAndSpace <*> parseCrlf

makeIndexedChords :: Int -> [(Chord, Int)] -> [IndexedChord]
makeIndexedChords _ [] = []
makeIndexedChords i ((c, s):xs) = IndexedChord i c : makeIndexedChords (i + chordLength c + s) xs

chordLength :: Chord -> Int
chordLength (SimpleChord k m) = keyLength k + Prelude.length m
chordLength (ChordWithBass k m b) = keyLength k + Prelude.length m + keyLength b

keyLength :: Entity.Tabs.Key -> Int
keyLength c = if c `Prelude.elem` [A, B, C, D, E, F, G] then 1 else 2

parseChordAndSpace :: Parser Char (Chord, Int)
parseChordAndSpace = (,) <$> parseChord <*> parseWhiteSpaces

parsePlainTextLine :: Parser Char String
parsePlainTextLine = const <$> parseText <*> parseCrlf

parseChord :: Parser Char Chord
parseChord = mkChord <$> parseKey <*> parseSignature <*> (((\_ k -> k) <$> token "/" <*> parseKey) <|> succeed InvalidKey)

mkChord :: Entity.Tabs.Key -> String -> Entity.Tabs.Key -> Chord
mkChord k s InvalidKey = SimpleChord k s
mkChord k s b          = ChordWithBass k s b

parseSignature :: Parser Char String
parseSignature = Prelude.concat <$> many (token "maj" <|> token "min" <|> token "m" <|> token "sus" <|> token "dim" <|> token "add"
        <|> token "2" <|> token "4" <|> token "5" <|> token "6" <|> token "7" <|> token "9" <|> token "11" <|> token "13")

char :: Char -> Parser Char Char
char c = satisfy (==c)

parseKey :: Parser Char Entity.Tabs.Key
parseKey = mkKey <$> satisfy isKey <*> (char '#' <|> char 'b' <|> succeed ' ')

isKey :: Char -> Bool
isKey c = c >= 'A' && c <= 'G'

mkKey:: Char -> Char -> Entity.Tabs.Key
mkKey 'A' '#' = ASharp
mkKey 'C' '#' = CSharp
mkKey 'D' '#' = DSharp
mkKey 'F' '#' = FSharp
mkKey 'G' '#' = GSharp
mkKey 'B' 'b' = ASharp
mkKey 'D' 'b' = CSharp
mkKey 'E' 'b' = DSharp
mkKey 'G' 'b' = FSharp
mkKey 'A' 'b' = GSharp
mkKey 'A' ' ' = A
mkKey 'B' ' ' = B
mkKey 'C' ' ' = C
mkKey 'D' ' ' = D
mkKey 'E' ' ' = E
mkKey 'F' ' ' = F
mkKey 'G' ' ' = G
mkKey _ _ = InvalidKey

parseWhiteSpaces :: Parser Char Int
parseWhiteSpaces = (Prelude.length <$> many (char ' ')) <|> succeed 0

parseCrlf :: Parser Char [Char]
parseCrlf = token "\n" <|> token "\r\n"

parseText :: Parser Char [Char]
parseText = many (satisfy (\c -> c /= '\n' && c /= '\r'))

run :: Parser a b -> [a] -> Maybe b
run p xs | Prelude.null (parse p xs) = Nothing
         | otherwise = Just (fst (Prelude.head (parse p xs)))


increment :: Entity.Tabs.Key -> Entity.Tabs.Key
increment A = ASharp
increment ASharp = B
increment B = C
increment C = CSharp
increment CSharp = D
increment D = DSharp
increment DSharp = E
increment E = F
increment F = FSharp
increment FSharp = G
increment G = GSharp
increment GSharp = A
increment InvalidKey = InvalidKey

raiseKey :: Int -> Entity.Tabs.Key -> Entity.Tabs.Key
raiseKey 0 k = k
raiseKey n k | n < 0 || n > 11 = raiseKey (n `mod` 12) k
             | otherwise       = raiseKey (n - 1) (increment k)

raiseChord :: Int -> IndexedChord -> IndexedChord
raiseChord n (IndexedChord i (SimpleChord k m))     = IndexedChord i (SimpleChord (raiseKey n k) m)
raiseChord n (IndexedChord i (ChordWithBass k m b)) = IndexedChord i (ChordWithBass (raiseKey n k) m (raiseKey n b))

transposeLine :: Int -> ChordTabLine -> ChordTabLine
transposeLine n (ChordTabLine t cs)    = ChordTabLine t (Prelude.map (raiseChord n) cs)

transposeContent :: Int -> TabContent -> TabContent
transposeContent n (ChordTabContent ls) = ChordTabContent (Prelude.map (transposeLine n) ls)
transposeContent _ _ = error "Only chord tabs can be transposed"

-- Parsing Tab type

data TabString = TabString Entity.Tabs.Key String deriving Show 

tabComponent :: Char -> Bool
tabComponent '-' = True
tabComponent '0' = True
tabComponent '1' = True
tabComponent '2' = True
tabComponent '3' = True
tabComponent '4' = True
tabComponent '5' = True
tabComponent '6' = True
tabComponent '7' = True
tabComponent '8' = True
tabComponent '9' = True
tabComponent 'h' = True
tabComponent 'p' = True
tabComponent 'b' = True
tabComponent '\\' = True
tabComponent '/' = True
tabComponent '~' = True
tabComponent _ = False

parseTabString :: Parser Char TabString
parseTabString = (\k _ s _ _ -> TabString k s)  <$> parseKey <*> char '|' <*> many (satisfy tabComponent) <*> char '|' <*> parseCrlf

data TabElement = TabData [TabString] | Section String deriving Show 

repeatParser :: Int -> Parser a b -> Parser a [b]
repeatParser 0 _ = succeed []
repeatParser n p = (:) <$> p <*> repeatParser (n - 1) p

parseTabElement :: Parser Char TabElement
parseTabElement = (TabData <$> repeatParser 6 parseTabString) <|> ((\_ t _ _ -> Section t) <$> char '[' <*> parseText <*> char ']' <*> parseCrlf)

parseEndOfFile :: Parser Char [Char]
parseEndOfFile = token "##EOF##"

uniformLength :: TabElement -> Bool
uniformLength (TabData xs) = sameLengthStrings xs
uniformLength _ = True

sameLengthStrings :: [TabString] -> Bool
sameLengthStrings [] = True
sameLengthStrings [_] = True
sameLengthStrings ((TabString _ s1) : x@(TabString _ s2) : xs) = Prelude.length s1 == Prelude.length s2 && sameLengthStrings (x:xs)

tabSectionKeys :: TabElement -> [Entity.Tabs.Key]
tabSectionKeys (TabData xs) = Prelude.map getStringKey xs
tabSectionKeys _ = []

validContent :: TabContent -> Bool
validContent (ChordTabContent _) = True
validContent (TabTabContent xs) = sameTuning (filter hasTuning xs) && Prelude.all uniformLength xs

hasTuning :: TabElement -> Bool
hasTuning (Section _) = False
hasTuning _           = True

sameTuning :: [TabElement] -> Bool
sameTuning [] = True
sameTuning [_] = True
sameTuning (x1 : x2 : xs) = tabSectionKeys x1 == tabSectionKeys x2 && sameTuning (x1 : xs)

getStringKey :: TabString -> Entity.Tabs.Key
getStringKey (TabString k _) = k

validate :: TabContent -> Maybe TabContent
validate content = if validContent content then Just content else Nothing
