{-# LANGUAGE OverloadedStrings #-}

import           AcceptRanges               hiding (main)
import           Control.Applicative
import           Control.Concurrent         (forkIO)
import           Control.Exception
import           Data.Attoparsec
import           Data.ByteString.Lazy       (ByteString, toStrict)
import qualified Data.ByteString.Lazy.Char8 as BC
import           Network
import           RequestParser
import           System.Environment         (getArgs)
import           System.IO                  (Handle, hClose, stdout)
import           System.IO.Unsafe
main :: IO ()
main = do
  args <- getArgs
  let num = extract args
  s <- listenOn (PortNumber $ fromIntegral num)
  putStrLn $ "Listening on " ++ show num
  socHandler s

socHandler :: Socket -> IO ()
socHandler soc = do
  (handle, _, _) <- accept soc
  forkIO $ process handle
  socHandler soc

process :: Handle -> IO ()
process handle = do
  dat <- BC.hGetContents handle
  let req = parseRequest (BC.toStrict dat)
  x <- unsafeInterleaveIO $ do
                   BC.hPutStrLn handle msg
                   BC.hPutStrLn stdout dat
                   print req
--                   mapM_ print $ g (toStrict dat)
  evaluate x
  hClose handle -- for http10 only... 1.1 can do persistent

-- http response
msg :: ByteString
msg = "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"

extract :: [String] -> Int
extract [] = 8080
extract [x] = read x
extract (x:_) = read x



















