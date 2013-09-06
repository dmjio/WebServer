module RequestTypes
    ( -- *  Request Type and methods
      Request
    , reqLine
    , reqHeaders
    , reqBody

      -- * RequestLine and accessor methods
    , RequestLine(RequestLine)
    , requestMethod
    , requestUri
    , requestVersion

    -- * HTTP Version
    , HTTPVersion(HTTP11,HTTP10,HTTP09)

    -- * Methods
    , Method(HEAD,GET, PUT, POST, DELETE, OPTIONS, CONNECT, TRACE)

    -- * URI Info for Routing
    , URI(AbURI, Asterisk, AbPath, Authority)
    ) where

import           Data.ByteString (ByteString)

-- | Request Data Type, contains a method, uri and version
data Request = Request
    { reqLine    :: RequestLine
    , reqHeaders :: [Header]
    , reqBody    :: !ByteString
    } deriving Show

data RequestLine = RequestLine
    { requestMethod  :: Method -- ^ Method
    , requestUri     :: URI -- ^ URI
    , requestVersion :: HTTPVersion -- ^ Version
    } deriving (Show)

data HTTPVersion = HTTP11 -- ^ HTTP 1.1 support
                 | HTTP10 -- ^ HTTP 1.0 support
                 | HTTP09 -- ^ HTTP 0.9 support
     deriving (Show)

data Method = HEAD | GET | PUT | POST | DELETE | OPTIONS | CONNECT | TRACE | Ext !ByteString
              deriving (Show,Eq, Ord)

data URI = Asterisk
         | AbURI !ByteString -- required req sent to a proxy
         | AbPath !ByteString
         | Authority !ByteString
           deriving (Show)

-- ^ Accept HTTP Header
data MediaRange = MediaRange { typ    :: !ByteString
                             , subTyp :: !ByteString
                             , param  :: [Param]
                             } deriving (Show)

data Param = Param {  name  :: !ByteString
                   ,  value :: Double
                   } deriving (Show)

-- HTTP Request Headers
data Header =
   Accept [MediaRange]
 | AcceptCharset
 | AcceptEncoding [Param]
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
