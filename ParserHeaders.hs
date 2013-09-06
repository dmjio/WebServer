{-# LANGUAGE OverloadedStrings #-}

module ParserHeaders where

import           Control.Applicative   (pure, (*>), (<$), (<$>), (<*), (<*>), (<|>))
import           Control.Monad
import           Data.Attoparsec       (Parser, parse, sepBy, string, try)
import           Data.Attoparsec.Char8 (char, double, endOfLine, satisfy, takeTill, takeWhile, takeWhile1)
import           Data.ByteString       (empty)
import           Data.ByteString.Char8 (ByteString, notElem, pack)
import           Data.Char             (chr, digitToInt)
import           Debug.Trace           (traceShow)
import           Prelude               hiding (notElem, takeWhile)
import           Request               (Header (Accept),MediaRange (MediaRange), Param (Param))
import           System.Environment    (getArgs)



-- | Helpers
spaces :: Parser ByteString
spaces = takeWhile (==' ')








