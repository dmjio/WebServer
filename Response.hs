{-# LANGUAGE OverloadedStrings #-}
module Response where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map              as M
import           Data.Maybe
import           MimeTypes
import           RFC2616

class ToResponse a where
    toResp :: a -> ByteString

(+++) :: ByteString -> ByteString -> ByteString
m +++ a = B.concat [m,a]

instance ToResponse Response where
    toResp (Response rv rc rmsg dat typ) = response
      where respHeader = rv +++ " " +++ rc +++ " " +++ rmsg
            respLength = "Content Length: " +++ (BC.pack.show $ B.length dat) +++ "\r\n"
            ctype = fromMaybe "" $ M.lookup typ mimeTypes
            respContentType = "Content-Type: " +++ ctype +++ "\r\n"
            response   = respHeader +++ "\r\n" +++ respLength +++ "\r\n" +++ dat




