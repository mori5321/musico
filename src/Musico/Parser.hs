module Musico.Parser
    ( Parser
    )
where

import           Text.Megaparsec                ( Parsec )
import           Data.Text                      ( Text )
import           Data.Void                      ( Void )

type Parser a = Parsec Void Text a
