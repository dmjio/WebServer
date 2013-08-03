{-# LANGUAGE OverloadedStrings #-}

module ParserHeaders where

import           Control.Applicative   hiding (many)
import           Data.Attoparsec       as P
import           Data.Attoparsec.Char8 as P8
import qualified Data.ByteString       as B
import           Data.ByteString.Char8 hiding (map, putStrLn)
import           Data.Char
import           Prelude               hiding (notElem)

token :: Parser Char
token = P8.satisfy $ \x ->
        notElem x $ pack (map chr $ 127 : [0..31])

data MediaRange = MediaRange { typ :: !ByteString,
                            subTyp :: !ByteString,
                             param :: [Param]
                             } deriving (Show)

data Param = Param {  name  :: !ByteString
                   ,  value :: Double
                   } deriving (Show)

data Header =
   Accept [MediaRange]
 | AcceptCharset
 | AcceptEncoding
 | AcceptLanguage
 | Authorization
 | Expect
 | From
 | Host
 | IfMatch
 | IfModifiedSince
 | IfNoneMatch
 | IfRange
 | IfUnmodifiedSince
 | MaxForwards
 | ProxyAuthorization
 | Range
 | Referer
 | TE
 | UserAgent
 deriving (Show)

-- accept = P8.takeWhile *> string "Accept:" *>
-- http://hackage.haskell.org/packages/archive/attoparsec/0.8.3.0/doc/html/Data-Attoparsec-Char8.html

main :: IO ()
main = print $ parse parseAccept str

str :: ByteString
str = "Accept: text/*;q=0.3, text/html;q=0.7, text/html;level=1,text/html;level=2;q=0.4, */*;q=0.5\n"

spaces = P8.takeWhile (==' ')

parseAccept :: Parser Header
parseAccept = string "Accept: " *> (Accept <$> P.sepBy mediaRange (char ',')) <* endOfLine

mediaRange :: Parser MediaRange
mediaRange = MediaRange <$> (spaces *> P8.takeWhile1 (/='/') <* char '/')
                        <*> P8.takeWhile1 (/= ';')
                        <*> (char ';' *> P.sepBy acceptParams (char ';'))

acceptParams :: Parser Param
acceptParams = Param <$> (P8.takeTill (=='=') <* char '=')
                     <*> double












