{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Control.Applicative   hiding (many)
import           Data.Attoparsec       as P
import           Data.Attoparsec.Char8 as P8
import           Data.ByteString.Char8 hiding (putStrLn)

-- GET / HTTP/1.1  <---- Request Line parser
-- Host: localhost:8080 <---- parsing host...
-- User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_4) AppleWebKit/536.30.1 (KHTML, like Gecko) Version/6.0.5 Safari/536.30.1
-- Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
-- Cache-Control: max-age=0
-- Accept-Language: en-us
-- Accept-Encoding: gzip, deflate
-- Connection: keep-alive

-- | Request Data Type, contains a method, uri and version
data Request = Request
    { requestMethod  :: Method -- ^ Method
    , requestUri     :: URI -- ^ URI
    , requestVersion :: HTTPVersion -- ^ Version
    } deriving (Show)

data HTTPVersion = HTTP11 -- ^ HTTP 1.1 support
                 | HTTP10 -- ^ HTTP 1.0 support
                 | HTTP09 -- ^ HTTP 0.9 support
     deriving (Show)

data Method = HEAD | GET | PUT | POST | DELETE | OPTIONS
            | CONNECT | TRACE | Ext !ByteString
                deriving (Show)

data URI = Asterisk
         | AbURI !ByteString -- required req sent to a proxy
         | AbPath !ByteString
         | Authority !ByteString
           deriving (Show)
           -- The authority form is used only by the connect message

-- | Request Parser
req :: Parser Method
req = P8.takeWhile (== ' ')  *>
         (GET <$ string "GET"
      <|> PUT <$ string "PUT"
      <|> POST <$ string "POST"
      <|> HEAD <$ string "HEAD"
      <|> TRACE <$ string "TRACE"
      <|> DELETE <$ string "DELETE"
      <|> OPTIONS <$ string "OPTIONS"
      <|> CONNECT <$ string "CONNECT") <* space
      -- ? extension

ver :: Parser HTTPVersion
ver = HTTP11 <$ string "HTTP/1.1"
 <|>  HTTP10 <$ string "HTTP/1.0"
 <|>  HTTP09 <$ string "HTTP/0.9" <* endOfLine

uri :: Parser URI
uri = AbURI <$> (string "http://" *> P8.takeWhile1 (/= ' ') <* space)
      <|> Asterisk <$ char '*' <* space
      <|> AbPath <$> (char '/' *> P8.takeWhile1 (/= ' ') <* space)
      <|> Authority <$> (char '/' *> P8.takeWhile1 (/= ' ') <* space)

--P8.space *> P8.takeWhile1 (/= ' ') <* char8 ' '

request :: Parser Request
request = Request <$> req <*> uri <*> ver

url :: ByteString
url = "OPTIONS * HTTP/1.1\n"

main :: IO ()
main = do
  case parse request url of
    Done _ y -> print y
    otherwise -> print "uh oh"





