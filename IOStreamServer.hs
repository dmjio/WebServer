{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import           Control.Monad.State
import qualified Data.ByteString.Char8            as C
import           Network.Socket                   hiding (recv)
import           Prelude                          hiding (takeWhile)
import           RequestTypes                     (Method (..))
import           RFC2616
import qualified System.IO.Streams                as Streams
import           System.IO.Streams.Attoparsec
import           System.IO.Streams.Network
import           System.Posix
import           Response
import           System.FilePath

main :: IO ()
main = withSocketsDo $ do
       installHandler sigPIPE Ignore Nothing 
       addrinfos <- getAddrInfo
                    (Just $ defaultHints {addrFlags = [AI_PASSIVE]})
                    Nothing (Just "3000")
       putStrLn $ "Listening on 3000"
       let serveraddr = head addrinfos
       sock <- socket (addrFamily serveraddr) Stream defaultProtocol
       bindSocket sock (addrAddress serveraddr)
       listen sock 1
       loop sock >> return ()

loop :: Socket -> IO ()
loop sock = do
  (con, _) <- accept sock
  forkIO $ reqResp con
  loop sock

reqResp :: Socket -> IO ()
reqResp conn = do
  (is,os) <- socketToStreams conn
  req <- parseFromStream request is
  resp <- createResponse req
  putStrLn  "====== REQUEST ====== "
  print req
  putStrLn  "====== RESPONSE ===== "
  print $ toResp resp
  Streams.write (Just $ toResp resp) os
  sClose conn

routes :: [((Method, FilePath), FilePath)]
routes  = [((GET,"/"), "index.html")
          ,((GET,"/favicon.ico"), "favicon.ico")]

lookupRoutes :: (Method,FilePath) -> Maybe FilePath
lookupRoutes mf = lookup mf routes

createResponse :: (Request,[Header]) -> IO Response
createResponse (x, hs) = do
    let resource = case lookupRoutes (requestMethod x, C.unpack $ (requestUri  x)) of
                  Just x -> x
                  Nothing -> ""
    if resource /= "" 
    then do file <- C.readFile resource
            return $! Response {
                           responseVersion = "HTTP/1.1"
                         , responseCode    = "200"
                         , responseMsg     = "OK"
                         , responseData    = file
                         , responseContentType = C.pack $ takeExtension resource
                         }
    else do fof <- C.readFile "404.html"
            return $! Response {
                           responseVersion = "HTTP/1.1"
                         , responseCode    = "404"
                         , responseMsg     = "Not Found"
                         , responseData    = fof
                         , responseContentType = C.pack $ takeExtension resource
                         }



