module Iteratee where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Monoid

data Chunk = Chunk
    { chunkData  :: !L.ByteString
    , chunkAtEOF :: !Bool
    } deriving (Show, Eq)

newtype Iter a = Iter { runIter :: Chunk -> Result a }

data Result a = Done { rResult :: a, rResidual :: Chunk }
              | NeedInput !(Iter a)
              | NeedIO !(IO (Result a))
              | Failed !SomeException

-- ^ example
readLine :: Iter (Maybe L.ByteString)
readLine = Iter (go L.empty)
    where go acc (Chunk input eof)
              | not (L.null b) = Done (Just acca) (Chunk btail eof)
              | not eof        = NeedInput (Iter (go acca))
              | otherwise      = Done Nothing (Chunk acca eof)
              where (a, b) = L8.break (== '\n') input
                    acca = L.append acc a
                    btail = L.tail b

instance Monoid Chunk where
    mempty = Chunk L8.empty False
    mappend (Chunk la eofa) (Chunk lb eofb) =
        Chunk (L.append la lb) (eofa || eofb)

instance Monad Iter where
    return a = Iter (Done a)
    m >>= k = Iter $ \c -> check (runIter m c)
        where check (Done a c)     = runIter (k a) c
              check (NeedInput m') = NeedInput (m' >>= k)
              check (NeedIO io)    = NeedIO (liftM check io)
              check (Failed e)     = Failed e
    fail msg = iterThrow (ErrorCall msg)

iterThrow :: (Exception e) => e -> Iter a
iterThrow e = Iter $ \_ -> Failed (toException e)

instance MonadPlus Iter where
    mzero = fail "mzero"
    mplus itera0 iterb  = go mempty itera0
        where go acc itera =
                  Iter $ \c ->
                  let acc' = mappend acc c
                      check (NeedInput i) = NeedInput (go acc' i)
                      check (NeedIO io) = NeedIO (liftM check io)
                      check (Failed _) = runIter iterb acc'
                      check r = r
                  in check $ runIter itera c

instance Show (Result a) where
    show (Done _ c) = "Done _ " ++ show c
    show (NeedInput _) = "NeedInput"
    show (NeedIO _) = "NeedIO"
    show (Failed e) = "Failed " ++ show e

instance MonadIO Iter where
    liftIO io = Iter $ \c -> NeedIO $ try io >>= mkResult c
        where mkResult _ (Left e)  = return (Failed e)
              mkResult c (Right a) = return (Done a c)

type Enumerator a = Iter a -> IO (Result a)
type Inum a       = Iter a -> Iter (Result a)

(.|) :: Inum a -> Iter a -> Iter a
(.|) inum iter = inum iter >>= getResult
infixr 4 .|

chunkEOF :: Chunk
chunkEOF = Chunk L.empty True

getResult0 :: Result a -> IO a
getResult0 (Done a _) = return a
getResult0 (NeedInput (Iter f)) = getResult0 (f chunkEOF)
getResult0 (NeedIO io) = io >>= getResult0
getResult0 (Failed e) = throwIO e

getResult :: (MonadIO m) => Result a -> m a
getResult (Done a _) = return a
getResult (NeedInput (Iter f)) = getResult (f chunkEOF)
getResult (NeedIO io) = liftIO io >>= getResult
getResult (Failed e) = liftIO $ throwIO e

run :: (MonadIO m) => Iter a -> m a
run = getResult . NeedInput

nlines1 :: Iter Int
nlines1 = go 0
    where go n = readLine >>= check n
          check n (Just _) = go $! n + 1
          check n Nothing  = return n

















