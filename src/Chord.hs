{-# LANGUAGE OverloadedStrings #-}

module Chord
    ( parseChord
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


import           Parser                         ( Parser )
import           Tone                           ( parseTone
                                                , Tone(..)
                                                , Accidental(..)
                                                , Note(..)
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

-- parseTest (parseChord <* eof) "Cmmmmm"
-- これでエラーを吐かせることができる

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

