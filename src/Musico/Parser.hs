module Musico.Parser
    ( Parser
    , parseMaybe
    , parse
    , parseTest
    )
where

import           Text.Megaparsec                ( Parsec
                                                , ParseErrorBundle
                                                )
import qualified Text.Megaparsec               as Megaparsec
                                                ( parse
                                                , parseTest
                                                )
import qualified Data.Text                     as T
                                                ( Text )
import           Data.Void                      ( Void )

type Parser a = Parsec Void T.Text a

parse :: Parser a -> T.Text -> Either (ParseErrorBundle T.Text Void) a
parse parser = Megaparsec.parse parser ""

parseTest :: (Show a) => Parser a -> T.Text -> IO ()
parseTest = Megaparsec.parseTest

parseMaybe :: Parser a -> T.Text -> Maybe a
parseMaybe parser text = do
    let parseResult = Megaparsec.parse parser "" text
    case parseResult of
        Left  _ -> Nothing
        Right a -> return a
