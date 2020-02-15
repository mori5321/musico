{-# LANGUAGE OverloadedStrings #-}

module Musico.Sample where


import           Text.Megaparsec                ( parseTest
                                                , parse
                                                )
import           Musico.Chord                   ( Chord
                                                , parseChord
                                                )
import           Musico.Parser                  ( Parser )
import qualified Data.Text                     as T
import           Data.Void                      ( Void )

sample1 :: IO ()
sample1 = parseTest parseChord "Cm7"

sample2 :: Maybe Chord
sample2 = do
    let result = parse parseChord "" "Cm7"
    case result of
        Left  _     -> Nothing
        Right chord -> return chord


