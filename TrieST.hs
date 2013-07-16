module TrieST (value, initialize, children, addchar, find, TrieST, trie) where

import Data.STRef
import Control.Monad.ST

data TrieST s a =
	Trie { value :: a
		 , children :: STRef s [(Char,STRef s (TrieST s a))]}

initialize :: a -> ST s (STRef s (TrieST s a))
initialize x = do
	chiln <- newSTRef []
	newSTRef $ Trie x chiln

trie :: a -> STRef s [(Char, STRef s (TrieST s a))] -> TrieST s a
trie = Trie

addchar :: Char -> a -> STRef s (TrieST s a) -> ST s ()
addchar ch x trieRef = do
	newTrie <- initialize x
	(Trie v chlnRef) <- readSTRef trieRef
	modifySTRef chlnRef ((ch,newTrie):)

find :: String -> STRef s (TrieST s a) -> ST s (Maybe a)
find []			  tr = readSTRef tr >>= return . Just . value
find (char:chars) tr = do
	t	  <- readSTRef tr
	-- t :: TrieST
	chiln <- readSTRef $ children t
	-- chiln :: [(Char,STRef s (TrieST s a))]
	case lookup char chiln of
		-- lookup char chiln :: Maybe (STRef s (TrieST s a))
		Nothing -> return Nothing
		Just tr' -> find chars tr'

subtrie :: String -> STRef s (TrieST s a) -> ST s (Maybe (TrieST s a))
subtrie []			 tr = readSTRef tr >>= return . Just
subtrie (char:chars) tr = do
	t	 <- readSTRef tr
	-- t :: TrieST s a
	chiln <- readSTRef $ children t
	-- chiln :: [(Char,STRef s (TrieST s a)]
	case lookup char chiln of
		-- lookup char chiln :: Maybe (STRef s (TrieST s a))
		Nothing -> return Nothing
		Just tr' -> subtrie chars tr'
