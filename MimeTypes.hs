{-# LANGUAGE OverloadedStrings #-}
module MimeTypes where

import           Data.ByteString (ByteString)
import           Data.Map        as M

type Mimes = M.Map ByteString ByteString

mimeTypes :: Mimes
mimeTypes = M.fromList
  [ (".js","text/javascript")
  , (".html","text/html")
  , (".css","text/css")
  , (".gif","image/gif")
  , (".jpg","image/jpeg")
  , (".png","image/png")
  , (".txt","text/plain")
  , (".doc","application/msword")
  , (".pdf","application/pdf")
  , (".zip","application/zip")
  , (".gz","application/x-gzip")
  , (".ps","application/postscript")
  , (".rtf","application/rtf")
  , (".ico","image/x-icon")
  ]
