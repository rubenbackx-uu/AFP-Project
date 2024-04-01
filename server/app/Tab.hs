module Tab where

data Tab = Tab { id :: Int, title :: String, artist :: String, content :: TabContent} deriving Show

newtype TabContent = ChordTab [ChordTabLine] deriving Show -- | TODO: Add Tab implementation here

data ChordTabLine = PlainTextLine String | ChordLine [IndexedChord] deriving Show

data IndexedChord = IndexedChord Int Chord deriving Show

data Chord = Chord Key Mode deriving Show  -- for instance, in a F#m7 chord F# is the Key, and m7 the Mode
type Mode = String

data Key = A | ASharp | B | C | CSharp | D | DSharp | E | F | FSharp | G | GSharp | InvalidKey deriving (Eq, Show)

-- Parsing
makeTab :: Int -> String -> String -> String -> Maybe Tab
makeTab i t a c = case run parseChordTab c of
        Nothing      -> Nothing
        Just con -> Just (Tab i t a con)

parseChordTab :: Parser Char TabContent
parseChordTab = ChordTab <$> many parseChordTabLine

parseChordTabLine :: Parser Char ChordTabLine
parseChordTabLine = parseChordLine <|> parsePlainTextLine

parseChordLine :: Parser Char ChordTabLine
parseChordLine = ChordLine <$> ((\i cs _ -> makeIndexedChords i cs) <$> parseWhiteSpaces <*> many parseChordAndSpace <*> parseCrlf)

makeIndexedChords :: Int -> [(Chord, Int)] -> [IndexedChord]
makeIndexedChords _ [] = []
makeIndexedChords i ((c, s):xs) = IndexedChord i c : makeIndexedChords (i + chordLength c + s) xs

chordLength :: Chord -> Int
chordLength (Chord k m) = keyLength k + length m

keyLength :: Key -> Int
keyLength c = if c `elem` [A, B, C, D, E, F, G] then 1 else 2

parseChordAndSpace :: Parser Char (Chord, Int)
parseChordAndSpace = (,) <$> parseChord <*> parseWhiteSpaces

parsePlainTextLine :: Parser Char ChordTabLine
parsePlainTextLine = (\t _ -> PlainTextLine t) <$> parseText <*> parseCrlf

parseChord :: Parser Char Chord
parseChord = Chord <$> parseKey <*> parseSignature

parseSignature :: Parser Char String
parseSignature = concat <$> many (token "maj" <|> token "min" <|> token "m" <|> token "sus" <|> token "dim" <|> token "/" <|> token "add"
        <|> token "2" <|> token "4" <|> token "5" <|> token "6" <|> token "7" <|> token "9" <|> token "11" <|> token "13" <|>
         token "A" <|> token "B" <|> token "C" <|> token "D" <|> token "E" <|> token "F" <|> token "G")

char :: Char -> Parser Char Char
char c = satisfy (==c)

parseKey :: Parser Char Key
parseKey = mkKey <$> satisfy isKey <*> (char '#' <|> char 'b' <|> succeed ' ')

isKey :: Char -> Bool
isKey c = c >= 'A' && c <= 'G'

mkKey:: Char -> Char -> Key
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
parseWhiteSpaces = (length <$> many (char ' ')) <|> succeed 0

parseCrlf :: Parser Char [Char]
parseCrlf = token "\n" <|> token "\r\n"

parseText :: Parser Char [Char]
parseText = many (satisfy (\c -> c /= '\n' && c /= '\r'))

run :: Parser a b -> [a] -> Maybe b
run p xs | null (parse p xs) = Nothing
         | otherwise = Just (fst (head (parse p xs)))
