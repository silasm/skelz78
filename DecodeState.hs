module DecodeState (initial, next) where

import Data.Array.IArray
import Control.Monad.State
import LZ78Constants

data DecodeState  = DecodeState {
	dictionary	:: Array Int String,
	writeNum	:: Int
	} deriving (Show, Read)

initial :: DecodeState
initial = DecodeState (array (0,dictSize) $ zip [0..dictSize] (repeat "")) 1

makestring :: Token -> DecodeState -> String
makestring (n,ch) (DecodeState d _) = d!n ++ [ch]

addstring :: String -> DecodeState -> DecodeState
addstring s (DecodeState d w) = DecodeState (d // [(w,s)]) (w + 1)

next :: Token -> State DecodeState String
next t = state $ \st -> 
	if writeNum st < dictSize
	then (s st, addstring (s st) st)
	else (s st,addstring (s st) initial)
	where
		s st = makestring t st
