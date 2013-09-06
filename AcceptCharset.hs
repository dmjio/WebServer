{-# LANGUAGE OverloadedStrings #-}

module AcceptCharset where

import           Control.Applicative   (pure, (*>), (<$), (<$>), (<*), (<*>),
                                        (<|>))
import           Control.Monad
import           Data.Attoparsec       (Parser, many1, parse, sepBy, string)
import           Data.Attoparsec.Char8 (char, double, endOfLine, satisfy,
                                        takeTill, takeWhile, takeWhile1, try)
import           Data.ByteString       (empty)
import           Data.ByteString.Char8 (ByteString, pack)
import           Data.Char             (isAlpha)
import           Prelude               hiding (notElem, takeWhile)
import           Request               (Header (Accept),
                                        MediaRange (MediaRange), Param (Param))
import           System.Environment    (getArgs)

tests :: [ByteString]
tests = [ "Accept-Encoding: compress, gzip\n"
        , "Accept-Encoding: \n"
        , "Accept-Encoding: *\n"
        , "Accept-Encoding: compress;q=0.5, gzip;q=1.0\n"
        , "Accept-Encoding: gzip;q=1.0, identity; q=0.5, *;q=0\n"
        , "Accept-Charset: iso-8859-5, unicode-1-1;q=0.8\n"
        ]

main :: IO ()
main = tt

tt :: IO ()
tt = forM_ tests $ \x -> print $ parse parseCharset x

-- starting over
-- parseCharset :: Parser [(ByteString,ByteString)]
parseCharset = do
  string "Accept-Encoding: " *> sepBy (getFields) (char ',') <* char '\n'
    where getFields = star <|> noq <|> hasq
          hasq = (,) <$> takeWhile (/='\n') <*> takeWhile (/='\n')
          star = spaces *> char '*' *> return ("*", empty) <* spaces
          noq  = (,) <$> (spaces *> (many1 $ satisfy isAlpha) <* spaces) <*> return empty


-- | Helpers

spaces :: Parser ByteString
spaces = takeWhile (==' ')














