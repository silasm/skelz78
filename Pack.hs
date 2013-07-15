module Pack (pack, pack_specialized, unpack, unpack_specialized) where

import LZ78Constants
import Data.Bits

-- packs a token into a single char, but may fail if the number of bits to
-- store the dictionary index plus the number of bits to store the character
-- exceeds 20, since
--
--	maxBound :: Char
--		==> \1114111
--	logBase 2 1114111
--		==> 20.087...
-- 
-- meaning Chars cannot store much past 20 bits. Not sure exactly why this is
-- (particularly the part where it /does/ go a little past 20 bits, so why not
-- just go to 21?), but I'll work with what I've got.
--
-- suggested use is with ByteStrings (each character is 8 bits) and dictionary
-- sizes of 4096 (12 bits long). This stays nicely within the bounds of the
-- Char type.
pack_specialized :: Token -> Char
pack_specialized (n,ch) = toEnum $ n + shiftL (fromEnum ch) numbits
	where
		numbits = ceiling $ logBase 2 $ fromIntegral dictSize

-- packs a token into two chars, one for the dictionary index, and another
-- for the the character that follows the string at that index. This is much
-- simpler and easier to understand, but of course wastes some space. In light
-- of the above considerations, it should be mentioned that this may also fail
-- for dictionaries larger than 1114111. Such dictionaries would require multiple
-- or variable numbers of Chars per index token, so I'll just be lazy and not
-- support them (larger dictionaries are prone to eating up a lot of memory
-- anyways, so maybe it's better that I don't).
pack :: Token -> [Char]
pack (n,ch)
	|n < maxChar = toEnum n : ch : []
	|otherwise	 = error "dictionaries of size greater than " ++ show maxChar ++
						 " cannot have their indeces packed into a char. " ++
						 "Either use a smaller dictionary, or define and use " ++
						 "a pack function other than the default. Sorry."
		where
			maxChar' :: Char
			maxChar' = maxBound
			maxChar :: Int
			maxChar = fromEnum maxChar'

unpack_specialized :: Char -> Token
unpack_specialized ch = (fromEnum ch `mod` 2^(numbits),
							toEnum $ shiftR (fromEnum ch) numbits)
	where
		numbits = ceiling $ logBase 2 $ fromIntegral dictSize

unpack :: [Char] -> Token
unpack [n,ch] = (fromEnum n, ch)
