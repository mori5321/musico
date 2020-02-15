{-# LANGUAGE OverloadedStrings #-}

module Musico.Tone
    ( parseTone
    , Note(..)
    , Accidental(..)
    , Tone(..)
    , toneToDegree
    , degreeToTone
    )
where
import           Text.Megaparsec                ( Parsec
                                                , choice
                                                , option
                                                )
import           Text.Megaparsec.Char           ( string )
import qualified Data.Text                     as T
import           Musico.Parser                  ( Parser )

data Note = C | D | E | F | G | A | B  deriving (Eq, Show)
data Accidental = Natural | Flat | Sharp deriving (Eq, Show)
data Tone = Tone Note Accidental deriving (Eq, Show)
type Degree = Int

parseNote :: Parser Note
parseNote = choice
    [ C <$ string "C"
    , D <$ string "D"
    , E <$ string "E"
    , F <$ string "F"
    , G <$ string "G"
    , A <$ string "A"
    , B <$ string "B"
    ]

parseAccidental :: Parser Accidental
parseAccidental =
    option Natural $ choice [Flat <$ string "b", Sharp <$ string "#"]

parseTone :: Parser Tone
parseTone = do
    note <- parseNote
    Tone note <$> parseAccidental

noteToDegree :: Note -> Degree
noteToDegree C = 0
noteToDegree D = 2
noteToDegree E = 4
noteToDegree F = 5
noteToDegree G = 7
noteToDegree A = 9
noteToDegree B = 11

accidentalToDegree :: Accidental -> Degree
accidentalToDegree Flat    = -1
accidentalToDegree Natural = 0
accidentalToDegree Sharp   = 1

toneToDegree :: Tone -> Degree
toneToDegree (Tone note accidental) = noteDegree + accidentalDegree
  where
    noteDegree       = noteToDegree note
    accidentalDegree = accidentalToDegree accidental

degreeToTone :: Degree -> Tone
degreeToTone x | x > 11    = degreeToTone' $ x `mod` 12
               | x < 0     = degreeToTone' $ 12 - 1
               | otherwise = degreeToTone' x

degreeToTone' :: Degree -> Tone
degreeToTone' 0  = Tone C Natural
degreeToTone' 1  = Tone D Flat
degreeToTone' 2  = Tone D Natural
degreeToTone' 3  = Tone E Flat
degreeToTone' 4  = Tone E Natural
degreeToTone' 5  = Tone F Natural
degreeToTone' 6  = Tone G Flat
degreeToTone' 7  = Tone G Natural
degreeToTone' 8  = Tone A Flat
degreeToTone' 9  = Tone A Natural
degreeToTone' 10 = Tone B Flat
degreeToTone' 11 = Tone B Natural
degreeToTone' _  = Tone C Natural
