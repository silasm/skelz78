module DecodeST (initialize, next) where

import Data.Array.ST.Safe
import Data.STRef
import Control.Monad.ST
import Control.Monad
import LZ78Constants

data DecodeST s = DecodeST {
	dictionary :: STArray s Int String,
	writeNum   :: STRef s Int
	}

initialize :: ST s (STRef s (DecodeST s))
initialize = do
	d <- newArray (0,dictSize) ""
	w <- newSTRef 1
	newSTRef $ DecodeST d w

reinitialize :: STRef s (DecodeST s) -> ST s ()
reinitialize dsr = do
	(DecodeST d w) <- readSTRef dsr
	forM_ [0..dictSize] $ \ix -> writeArray d ix ""
	writeSTRef w 1

makestring :: Token -> STRef s (DecodeST s) -> ST s String
makestring (n,ch) dsr = do
	(DecodeST d _) <- readSTRef dsr
	prefix <- readArray d n
	return $ prefix ++ [ch]

addstring :: String -> STRef s (DecodeST s) -> ST s ()
addstring str dsr = do
	(DecodeST dict wnr) <- readSTRef dsr
	wn <- readSTRef wnr
	writeArray dict wn str
	modifySTRef wnr (+1)

next :: Token -> STRef s (DecodeST s) -> ST s String
next t dsr = do
	(DecodeST dict wnr) <- readSTRef dsr
	wn <- readSTRef wnr
	str <- makestring t dsr
	if wn < dictSize
	then do
		addstring str dsr
		return str
	else do
		reinitialize dsr
		addstring str dsr
		return str
