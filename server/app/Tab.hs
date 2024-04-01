module Tab where

data Tab = Tab { id :: Int, title :: String, artist :: String, content :: TabContent} deriving Show

newtype TabContent = ChordTab [ChordTabLine] deriving Show -- | TODO: Add Tab implementation here

data ChordTabLine = PlainTextLine String | ChordLine [IndexedChord] deriving Show

data IndexedChord = IndexedChord Int Chord deriving Show

data Chord = Chord Key Mode deriving Show  -- for instance, in a F#m7 chord F# is the Key, and m7 the Mode
type Mode = String

data Key = A | ASharp | B | C | CSharp | D | DSharp | E | F | FSharp | G | GSharp | InvalidKey deriving (Eq, Show)
