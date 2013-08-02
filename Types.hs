{-# LANGUAGE OverloadedStrings #-}

module Types where

import qualified Data.ByteString       as B
import           Data.ByteString.Char8 as B8

-- | Http Url Schemes
data UrlScheme = HTTP | HTTPS  deriving (Show, Eq) -- ^ URL Scheme

-- | Http Methods with helpers
data Method = OPTIONS -- ^ URL Methods
            | GET
            | HEAD
            | POST
            | PUT
            | DELETE
            | TRACE
            | CONNECT -- ^ reserved for future use
            | Method B.ByteString
  deriving (Show, Read)

mToBS :: Method -> ByteString
mtoBS (Method bs) = bs -- ^ Double check this
mToBS = B8.pack . show

bsToM :: ByteString -> Method
bsToM bs
    | B8.length bs <= 7 = read . B8.unpack $ bs
    | otherwise         = Method bs

-- | HTTP Version decalarations and helpers
data HttpVersion = HTTP10 | HTTP11 deriving (Show, Eq)

-- | Req Headers, can be sent in any order
data RequestHeader =
      Accept -- ^ Tells what MIME type it's willing to accept
    | AcceptCharset -- ^ Default is to accept any charset
    | AcceptEncoding -- ^ decides encoding and compression
    | AcceptLanguage -- ^ What languages are supported
    | Authorization -- ^ Required after received a 401 unauthorized
    | Cookie
    | Connection
    | CacheControl
    | ETag
    | UserAgent
    | ReqContentLength
    | ReqContentType
    | Host -- ^ Mandatory in each request as of HTTP/1.1
    | Referer
    | RequestHeader B.ByteString
    deriving (Show, Read)

-- instance Eq RequestHeader where x == y = requestHeaderToBS x == requestHeaderToBS y

-- ^ Response Headers
data ResponseHeader =
      ContentEncoding
    | ContentLanguage
    | ContentLength
    | ContentDisposition
    | ContentType
    | Expires
    | Location
    | Server
    | SetCookie
    | ResponseHeader B.ByteString
     deriving (Show)

-- | Status Codes
data Status =
      Status200
    | Status201 -- ^ Request succesful, resulted in a resource being created
    | Status202 -- ^ Accepted
    | Status301 -- ^ Moved Permanently
    | Status302 -- ^ Found
    | Status303 -- ^ See Other
    | Status400 -- ^ Bad Request
    | Status401 -- ^ Unauthorized
    | Status402 -- ^ Payment Required (future use...)
    | Status403 -- ^ Forbidden
    | Status404 -- ^ Not Found
    | Status405 -- ^ Method not allowed
    | Status500 -- ^ Internal Server Error
    | Status Int B.ByteString
    deriving Show

--- | Information on the request sent by the client. This abstracts
--- away the
-- details of the underlying implementation.
data Request = Request
  {  requestMethod  :: Method -- ^ Get
  ,  httpVersion    :: HttpVersion
  ,  pathInfo       :: B.ByteString
  -- | If no query string was specified, this should be empty.
  ,  queryString    :: B.ByteString
  ,  serverName     :: B.ByteString
  ,  serverPort     :: Int
  ,  requestHeaders :: [(RequestHeader, B.ByteString)]
  ,  urlScheme      :: UrlScheme
--  ,  requestBody    :: Source
  ,  errorHandler   :: String -> IO ()
  -- | The client\'s host information.
  ,  remoteHost     :: B.ByteString
  }

data Response = Response
  { status          :: Status
  , responseHeaders :: [(ResponseHeader, B.ByteString)]
  --  , responseBody    :: Either FilePath Enumerator
}

type Application = Request -> IO Response
type Middleware = Application -> Application




