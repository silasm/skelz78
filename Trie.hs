module Trie (value, children, addword, find, Trie, trie) where

data Trie a =
	Trie { value	:: Maybe a
		 , children :: [(Char, Trie a)] } deriving (Show, Read)

trie :: Maybe a -> [(Char, Trie a)] -> Trie a
trie = Trie

addword :: String -> a -> Trie a -> Trie a
addword []	  val t = Trie (Just val) (children t)
addword chars val t = Trie (value t) (addchar chars val (children t))

addchar :: String -> a -> [(Char, Trie a)] -> [(Char, Trie a)]
addchar (char:chars) val []			= [(char, addword chars val $ Trie Nothing [])]
addchar (char:chars) val ((char',trie):chiln)
	|char' == char = (char, addword chars val trie):chiln
	|otherwise	   = (char',trie):addchar (char:chars) val chiln

find :: String -> Trie a -> Maybe a
find []			  t = value t
find (char:chars) t = do
						t' <- lookup char $ children t
						find chars t'

subtrie :: String -> Trie a -> Maybe (Trie a)
subtrie []			 t = Just t
subtrie (char:chars) t = do
							t' <- lookup char $ children t
							subtrie chars t'
