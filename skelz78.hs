module Main where
import LZ78
import Pack
import System.Environment

main :: IO ()
main = do
	args <- getArgs
	contents <- getContents
	case args of
		("-do":[])	   -> putStr $ decode $ map unpack_specialized contents
		("-od":[])	   -> putStr $ decode $ map unpack_specialized contents
		("-o":"-d":[]) -> putStr $ decode $ map unpack_specialized contents
		("-d":"-o":[]) -> putStr $ decode $ map unpack_specialized contents
		("-o":[])	   -> putStr $ map pack_specialized $ encode contents
		("-d":[])	   -> putStr $ decode $ map unpack $ group2 $ contents
		("-h":[])	   -> putStrLn help
		o:_			   -> error $ "unrecognized or duplicate options"
		[]			   -> putStr $ concat $ map pack $ encode contents

help :: String
help =	"skelz78 1.0\nUsage:\n\n\tskelz [-d][-o] < inputFile > outputFile\n"++
		"To compress a file normally\n\n\tskelz <inputFile > outputFile\n\n"++
		"To decompress a file compressed this way\n\n\tskelz -d < inputFile"++
		" > outputFile\n\nOptions:\n\n\t -o : compresses tokens into a "	++
		"single character, but may fail. Recommended only to use on ASCII " ++
		"with dictionary sizes not exceeding 4096.\n\t -d : decompresses a "++
		"file. Use -o with files that were compressed with -o to decompress"++
		" appropriately."

group2 :: [a] -> [[a]]
group2 (x:x':xs) = [x,x']:group2 xs
group2 (x:[])	 = [[x]]
group2 []		 = []
