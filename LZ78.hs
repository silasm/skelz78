module LZ78 (encode, decode) where
import qualified EncodeState as E
import qualified DecodeState as D
import Control.Monad.State
import LZ78Constants
import Data.Maybe


encode :: String -> [Token]
encode str = catMaybes $ extract (last str) $ runState (mapM E.next (init str)) E.initial
	where
		extract :: Char -> ([Maybe Token],E.EncodeState) -> [Maybe Token]
		extract ch (mts,st) = mts ++ [Just $ E.maketoken ch st]


decode :: [Token] -> String
decode ts = concat $ evalState (mapM D.next ts) D.initial
