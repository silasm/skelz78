module EncodeState (initial, next, EncodeState, maketoken) where

import Trie
import Control.Monad.State
import LZ78Constants

data EncodeState = EncodeState { -- an encoding state consists of
					-- a dictionary for keeping track of previously encountered
					-- strings
					topTrie  :: Trie Int,
					-- our current location in that dictionary, traversed character
					-- by character
					curTrie	 :: Trie Int,
					-- the current string being built, which must a member of the
					-- dictionary
					curStr	 :: String,
					-- the current size of the dictionary in terms of number of
					-- strings stored
					writeNum :: Int
					-- the maximum size of the dictionary in terms of number of
					-- strings stored
					} deriving (Show, Read)

-- the encoding state starts off at the top of an empty trie, with no string being
-- built, and only the empty string in the dictionary
initial :: EncodeState
initial = let e = trie (Just 0) [] in EncodeState e e "" 1

-- helper function to simplify the process of making a new encoded token to be placed
-- in the output.
maketoken :: Char -> EncodeState -> Token
maketoken ch st =
	case value $ curTrie st of
		Just n  -> (n,ch)
		Nothing -> error "invalid prefix in encoding"

next :: Char -> State EncodeState (Maybe Token)
next ch = state $ \st ->
	-- if the dictionary isn't full
	if writeNum st < dictSize then
	-- check if the next char forms a new token
	case lookup ch (children $ curTrie st) of
		-- if it's already found in this state (as a child path of this node in the
		-- trie)
		Just t' ->
			-- return nothing and recur down that path in the state
			(Nothing, EncodeState (topTrie st) t' (ch:curStr st) (writeNum st))
		-- if it's not found in this state
		Nothing ->
			-- return the new token and add it to the state
			(Just (maketoken ch st), EncodeState newTrie newTrie "" (wn + 1))
				where
					str = reverse $ ch : curStr st
					wn = writeNum st
					newTrie = addword str wn $ topTrie st
	-- if the dicitionary is full, add the new char as a token to an empty dictionary
	else let e = addword [ch] 1 $ trie (Just 0) [] in
		(Just (maketoken ch initial), EncodeState e e "" 2)
