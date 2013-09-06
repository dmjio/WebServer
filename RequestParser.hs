{-# LANGUAGE OverloadedStrings #-}

module RequestParser
    (  parseRequest
     , Headers
    ) where

import           Control.Applicative   hiding (many)
import           Data.Attoparsec       as P
import           Data.Attoparsec.Char8 as P8
import           Data.ByteString.Char8 as C hiding (putStrLn)
import           RequestTypes

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

ver :: Parser HTTPVersion
ver = HTTP11 <$ string "HTTP/1.1"
 <|>  HTTP10 <$ string "HTTP/1.0"
 <|>  HTTP09 <$ string "HTTP/0.9"

spaces :: Parser ByteString
spaces = P8.takeWhile (==' ')

uri :: Parser URI
uri = AbURI <$> P.takeWhile1 (/=32) <* char8 ' '

--  AbURI     <$> urlParser <|>
  --     Asterisk  <$  astParser <|>
  --     AbPath    <$> abParser  <|>
  --     Authority <$> authParser
  -- where
  --   urlParser = spaces *> string "http://" *> P8.takeWhile1 (/= ' ') <* space
  --   astParser = spaces *> char '*' <* spaces
  --   abParser  = spaces *> char '/' *> P8.takeWhile1 (/= ' ') <* space
  --   authParser= spaces *> char '/' *> P8.takeWhile1 (/= ' ') <* space

parseReqLine :: ByteString -> RequestLine
parseReqLine x = case (parse (RequestLine <$> req <*> uri <*> ver <* endOfLine) x) of
                   Done x y -> y
                   otherwise -> error "uh oh"

bs :: ByteString
bs = "GET / HTTP/1.1\n\
      \Host: localhost:8080\n\
      \User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_4) AppleWebKit/536.30.1      (KHTML, like Gecko) Version/6.0.5 Safari/536.30.1\n\
      \Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\n\
      \Accept-Language: en-us\n\
      \Accept-Encoding: gzip, deflate\n\
      \Connection: keep-alive\n"

url :: ByteString
url = "GET / HTTP/1.0\n"

main = print $ parseReqLine url

verbTest = Prelude.map (parse req) ["GET ","POST ", "PUT ",
                                    "HEAD ", "TRACE ", "DELETE "]
versionTest = Prelude.map (parse ver) ["HTTP/1.1 ", "HTTP/1.0 ", "HTTP/0.9" ]

uriTest = Prelude.map (parse uri) ["* "]

-- | Parse Request Headers
type Headers = [(ByteString, ByteString)]

parseRequest :: ByteString -> (RequestLine, Headers)
parseRequest xs = let (hd:body) = split '\n' xs
                  in (,) (parseReqLine hd) (parseHeaders body)

parseHeaders :: [ByteString] -> [(ByteString, ByteString)]
parseHeaders xs = Prelude.map parser (Prelude.init . Prelude.init $ xs)
    where parser z = case parse content z of
                       Done x y -> y
                       otherwise -> error "parse error"

content :: Parser (ByteString, ByteString)
content = (,) <$> (P8.takeWhile (/=':') <* char ':')
              <*> (spaces *> P8.takeWhile (/='\r') <* char '\r')






