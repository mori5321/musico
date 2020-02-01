{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tutorial where

import           Text.Megaparsec                ( Parsec
                                                , parseTest
                                                , satisfy
                                                , choice
                                                , optional
                                                , some
                                                , try
                                                , (<?>)
                                                , label
                                                )
import           Text.Megaparsec.Char           ( char
                                                , string
                                                , alphaNumChar
                                                )
import           Text.Megaparsec.Debug          ( dbg )
import           Control.Monad.Identity         ( Identity
                                                , void
                                                )
import           Data.Text
import qualified Data.Text                     as T
import           Data.Void                      ( Void )
import qualified Text.Megaparsec.Char.Lexer    as L
import           Control.Applicative            ( (<|>) )

type Parser a = Parsec Void Text a

data Scheme
        = SchemeData
        | SchemeFile
        | SchemeFtp
        | SchemeHttp
        | SchemeHttps
        | SchemeIrc
        | SchemeMailto
        deriving (Eq, Show)

pScheme :: Parser Scheme
pScheme = choice
    [ SchemeData <$ string "data"
    , SchemeFile <$ string "file"
    , SchemeFtp <$ string "ftp"
    , SchemeHttps <$ string "https"
    , SchemeHttp <$ string "http"
    , SchemeIrc <$ string "irc"
    , SchemeMailto <$ string "mailto"
    ]

data Uri = Uri
  { uriScheme :: Scheme
  , uriAuthority :: Maybe Authority
  } deriving (Eq, Show)

data Authority = Authority
  { authUser :: Maybe (Text, Text)
  , authHost :: Text
  , authPort :: Maybe Int
  } deriving (Eq, Show)

pUri :: Parser Uri
pUri = do
    uriScheme <- dbg "scheme" pScheme <?> "valid scheme"
    void (char ':')
    uriAuthority <- dbg "auth" . optional $ do
        void (string "//")
        authUser <- dbg "user" . optional . try $ do
            user <- T.pack <$> some alphaNumChar <?> "username"
            void (char ':')
            password <- T.pack <$> some alphaNumChar <?> "password"
            void (char '@')
            return (user, password)
        authHost <-
            T.pack
            <$> dbg "host" (some (alphaNumChar <|> char '.'))
            <?> "hostname"
        authPort <- dbg "port" $ optional (char ':' *> label "port" L.decimal)
        return Authority { .. }
    return Uri { .. }

