{-# LANGUAGE OverloadedStrings #-}
module Accept where

import           Control.Applicative   hiding (many)
import           Control.Monad         (forM_)
import           Data.Attoparsec       as P
import           Data.Attoparsec.Char8 as P8
import           Data.ByteString.Char8 hiding (putStrLn)
import           RequestTypes

-- | Parse Accept Header

spaces :: Parser ByteString
spaces = P8.takeWhile (==' ')

reqHeadTests :: IO ()
reqHeadTests = forM_ tests $ \x -> print $ parse parseAccept x

tests = [str1,str2,str3,str4,str5]

str1,str2,str3,str4,str5 :: ByteString
str1 = "Accept: text/*;q=0.3, text/html;q=0.7, text/html;level=1,text/html;level=2;q=0.4, */*;q=0.5\n"
str2 = "Accept: */*\n"
str3 = "Accept: audio/*; q=0.2, audio/basic\n"
str4 = "Accept: text/plain; q=0.5, text/html,text/x-dvi; q=0.8, text/x-c\n"
str5 = "Accept: \n"

parseAccept :: Parser Header
parseAccept = Accept <$> (string "Accept: " *> sepBy mediaRange (char ',') <* endOfLine)

mediaRange :: Parser MediaRange
mediaRange = MediaRange <$> (spaces *> P8.takeWhile1 (/='/') <* char '/')
                        <*> P8.takeWhile1 (\x -> x /= ';' && x /= '\n')
                        <*> (char ';' *> spaces *> sepBy acceptParams (char ';')
                        <|> pure [])

acceptParams :: Parser Param
acceptParams = Param <$> (P8.takeTill (=='=') <* char '=') <*> double




