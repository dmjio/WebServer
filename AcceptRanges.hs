{-# LANGUAGE OverloadedStrings #-}

module AcceptRanges where

import           Control.Applicative   ((*>), (<$), (<$>), (<*), (<*>), (<|>))
import           Control.Monad         (forM_)
import           Data.Attoparsec       (Parser, many1, parse, string, takeTill,
                                        try)
import           Data.Attoparsec.Char8 (anyChar, char, double, endOfLine,
                                        isEndOfLine, satisfy, sepBy, takeWhile)
import           Data.ByteString.Char8 (ByteString, empty, head, pack, split,
                                        tail)
import           Data.Char             (chr, isAlpha)
import           Data.List             (notElem)
import           Prelude               hiding (notElem, tail, takeWhile)
import           System.Environment    (getArgs)



-- Accept-Ranges     = "Accept-Ranges" ":" acceptable-ranges
-- acceptable-ranges = 1#range-unit | "none"

tests :: [ByteString]
tests = [bytes,none]

bytes, none :: ByteString
bytes = "Accept-Ranges: bytes\n"
none  = "Accept-Ranges: none\n"

main :: IO ()
main = do
  tt
  

tt :: IO ()
tt = forM_ tests $ \x -> print $ parse parseRanges x

spaces :: Parser ByteString
spaces = takeWhile (==' ')

-- Accept - Range
parseRanges :: Parser ByteString
parseRanges = string "Accept-Ranges: " *> takeTill isEndOfLine <* endOfLine

-- Age
parseAge :: Parser Double
parseAge = string "Age: " *> double
t :: IO ()
t = print $ parse parseAge "Age: 31\n"

-- Allow
parseAllow :: Parser [String]
parseAllow = string "Allow: " *> sepBy (spaces *> word) (char ',') <* endOfLine
    where word = many1 (satisfy isAlpha)

k :: IO ()
k = print $ parse parseAllow "Allow: GET, PUT, HEAD\n"

-- Host
host :: Parser (ByteString, ByteString)
host = string "Host: " *> body where
    body = (,) <$> hName <*> port
    hName = takeWhile (\x -> x /= '\n' && x /= ':')
    port  = let end     = spaces *> endOfLine
                noPort  = empty <$ end
                hasPort = char ':' *> spaces *> takeTill isEndOfLine <* endOfLine
            in  noPort <|> hasPort

hostTest :: IO ()
hostTest = forM_ hostTests $ \x -> print $ parse host x

hostTests :: [ByteString]
hostTests = ["Host: www.w3.org\n","Host: localhost:8080\n", "Host: \n"]

-- Accept-Encoding
encTest :: IO ()
encTest = forM_ encTests $ \x -> print $ parse acceptEncoding x

encTests :: [ByteString]
encTests = ["Accept-Encoding: gzip,deflate,sdch\n"] ++
           ["Accept-Encoding: gzip, deflate, sdch\n"] ++
           ["Accept-Encoding: gzip\n"] ++
           ["Accept-Encoding: \n"]

acceptEncoding :: Parser [String]
acceptEncoding = string "Accept-Encoding: " *> spaces *> inp
    where inp  = sepBy word (char ',') <* endOfLine
          word = spaces *> many1 (satisfy isAlpha)

-- Accept Language
-- ...

-- User Agent...
utests = ["User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:12.0) Gecko/20100101 Firefox/21.0\n"] ++ ["User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/27.0.1453.116 Safari/537.36\n"] ++ ["User-Agent: CERN-LineMode/2.15 libwww/2.17b3"]

us = forM_ utests $ \x -> print $ parse userAgent x

userAgent :: Parser [(ByteString,ByteString)]
userAgent = string "User-Agent: " *> spaces *> sepBy p (char ' ')
    where p = (,) <$> takeWhile (\x -> x/=' ') <*> try (com <|> nocom)
          com = char ' ' *> char '(' *> takeWhile (/= ')') <* char ')'
          nocom = char ' ' *> return empty

token :: Parser Char
token = satisfy $ \x -> notElem x $ map chr $ 127 : [0..31]

connection :: Parser ByteString
connection = string "Connection: " *> spaces *> takeTill isEndOfLine <* endOfLine

con :: IO ()
con = print $ parse connection "Connection: keep-alive\n"











