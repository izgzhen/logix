import Tokenizer
import Parser

main = do
	tblocks <- readTokens "example"
	print $ map parseBlock tblocks