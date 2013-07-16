module EncodeST (initialize, next, maketoken, extract) where

import qualified TrieST as T
import Control.Monad.ST
import Data.STRef
import LZ78Constants

data EncodeST s = EncodeST {
	topTrie :: STRef s (T.TrieST s Int),
	curTrie :: STRef s (T.TrieST s Int),
	writeNum :: STRef s Int
	}

initialize :: ST s (STRef s (EncodeST s))
initialize = do
	tt <- T.initialize 0
	wn <- newSTRef 1
	newSTRef $ EncodeST tt tt wn

reinitialize :: STRef s (EncodeST s) -> ST s ()
reinitialize esr = do
	(EncodeST tt ct wn) <- readSTRef esr
	emptyRef <- newSTRef []
	writeSTRef tt (T.trie 0 emptyRef)
	writeSTRef ct (T.trie 0 emptyRef)
	writeSTRef wn 1

maketoken :: Char -> STRef s (EncodeST s) -> ST s Token
maketoken ch esr = do
	(EncodeST ttr ctr wnr) <- readSTRef esr
	ct <- readSTRef ctr
	wn <- readSTRef wnr
	T.addchar ch wn ctr
	modifySTRef wnr (+1)
	return (T.value ct,ch)

next :: Char -> STRef s (EncodeST s) -> ST s (Maybe Token)
next ch esr = do
	(EncodeST ttr ctr wnr) <- readSTRef esr
	wn <- readSTRef wnr
	if wn < dictSize
	then do
		ct <- readSTRef ctr
		chiln <- readSTRef $ T.children ct
		case lookup ch chiln of
			Just ref -> writeSTRef esr (EncodeST ttr ref wnr) >> return Nothing
			Nothing  -> maketoken ch esr >>= return . Just
	else do
		reinitialize esr
		token <- maketoken ch esr
		return $ Just token


extract :: Char -> STRef s (EncodeST s) -> [Maybe Token] -> ST s [Maybe Token]
extract ch esr mts = do
	token <- maketoken ch esr
	return $ mts ++ [Just token]
