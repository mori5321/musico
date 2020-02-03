{-# LANGUAGE OverloadedStrings #-}

module Musico.Tone
    ( parseTone
    , Note(..)
    , Accidental(..)
    , Tone(..)
    )
where
import           Text.Megaparsec                ( Parsec
                                                , choice
                                                , option
                                                )
import           Text.Megaparsec.Char           ( string )
import qualified Data.Text                     as T
import           Musico.Parser                         ( Parser )

data Note = C | D | E | F | G | A | B  deriving (Eq, Show)
data Accidental = Natural | Flat | Sharp deriving (Eq, Show)
data Tone = Tone Note Accidental deriving (Eq, Show)

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
