{-# LANGUAGE OverloadedStrings #-}

module ParseHeader where

import           Control.Applicative   hiding (many)
import           Data.Attoparsec       as P
import           Data.Attoparsec.Char8 as P8
import           Data.ByteString.Char8 hiding (putStrLn)

data Header =
   Accept
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



