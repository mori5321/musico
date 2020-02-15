{-# LANGUAGE OverloadedStrings #-}

-- TODO:
-- DirecotyをMusico.Chordのように分ける。
-- runParser, parseTestなどの関数をWrapして提供する

module Musico.Chord
    ( parseChord
    , Chord
    , toTones
    )
where

import           Text.Megaparsec                ( Parsec
                                                , parseTest
                                                , satisfy
                                                , choice
                                                , option
                                                , optional
                                                , some
                                                , try
                                                , (<?>)
                                                , label
                                                , eof
                                                )
import           Text.Megaparsec.Char           ( char
                                                , string
                                                , alphaNumChar
                                                )
import           Text.Megaparsec.Debug          ( dbg )
import           Control.Monad.Identity         ( Identity
                                                , void
                                                )
import qualified Data.Text                     as T
import qualified Text.Megaparsec.Char.Lexer    as L
import           Control.Applicative            ( (<|>) )

import           Musico.Parser                  ( Parser )
import           Musico.Tone                    ( parseTone
                                                , Tone(..)
                                                , Accidental(..)
                                                , Note(..)
                                                , toneToDegree
                                                , degreeToTone
                                                )

data Quality = Major | Minor | Augment | Diminish deriving (Show, Eq)
data Seventh = No7th | Major7th | Minor7th | Dominant7th | Diminish7th deriving (Show, Eq)
data Tension = NoTension | Tention9th | Tension11th | Tension13th deriving (Show, Eq)

data Chord = Chord { root :: Tone
                   , quality :: Quality
                   , seventh :: Seventh
                   , tensions :: [Tension]
                   } deriving (Show)

parseChord' :: Parser Chord
parseChord' = do
    rootTone <- parseTone
    quality  <- parseQuality
    seventh  <- reDefine7th quality <$> parseSeventh
    return $ Chord rootTone quality seventh []

parseChord :: Parser Chord
parseChord = parseChord' <* eof

parseQuality :: Parser Quality
parseQuality = option Major $ choice
    [Minor <$ string "m", Augment <$ string "aug", Diminish <$ string "dim"]


reDefine7th :: Quality -> Seventh -> Seventh
reDefine7th Diminish Minor7th = Diminish7th
reDefine7th Major    Minor7th = Dominant7th
reDefine7th _        seventh  = seventh


parseSeventh :: Parser Seventh
parseSeventh =
    option No7th $ choice [Major7th <$ string "M7", Minor7th <$ string "7"]

toTones :: Chord -> [Tone]
toTones chord = [rootTone, midTone, fifthTone, seventhTone]
  where
    rootTone    = root chord
    midTone     = degreeToTone $ (+ 4) $ toneToDegree rootTone --Tmp
    fifthTone   = degreeToTone $ (+ 7) $ toneToDegree rootTone --Tmp
    seventhTone = degreeToTone $ (+ 11) $ toneToDegree rootTone --Tmp

-- Example
-- let chord = parse parseChord "Cm7"
-- toTones <$> chord
