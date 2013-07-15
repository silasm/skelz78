module LZ78Constants (dictSize, Token) where

-- tokens hold all the information that will be kept in the encoded string: the
-- index in the dictionary of the previously encountered string, and the newly
-- encountered character which was not a suffix of that string.
type Token = (Int,Char)


dictSize :: Int
dictSize = 4096
