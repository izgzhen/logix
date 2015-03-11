module PropParser where
import Tokenizer
import Data.Map

data Formula = Imply Formula Formula | Term String | Empty deriving (Eq)

instance Show Formula where
	show (Term str) = str
	show (Imply front end) = "(" ++ show front ++ " -> " ++ show end ++ ")"
	show Empty = ""


parseLine :: String -> String -- Input and Response
parseLine ln = case (head tks) of
	TkKey Axiom -> show $ parseAxiom $ tail tks
	TkKey Check -> parseCheck $ tail tks
	TkTac _     -> parseTac tks
	TkIdent _   -> parseProof tks
  where
  	tks = tokenize ln

parseCheck :: [Token] -> String
parseCheck = undefined

parseTac :: [Token] -> String
parseTac = undefined

parseProof :: [Token] -> String
parseProof = undefined

parseAxiom :: [Token] -> (String, [String], Formula)
parseAxiom (TkIdent name : TkSyn LP : tks) = (name, args, body)
	where
		(args, rest) = parseArgs $ tks
		(body, []) = parseFormula $ tail rest
parseAxiom _ = error "Error parsing axiom"

parseArgs :: [Token] -> ([String], [Token])
parseArgs [] = ([],[])
parseArgs (TkIdent ident : TkSyn Comma : tks) = (ident : args, rest)
	where (args, rest) = parseArgs tks
parseArgs (TkIdent ident : TkSyn RP : tks) = ([ident], tks)
parseArgs tks = error $ "Error parsing args" ++ show tks


parseFormula :: [Token] -> (Formula, [Token])
parseFormula [] = (Empty, [])
parseFormula (TkIdent ident : TkSyn Curry : tks)
	| length tks > 0 = (Imply (Term ident) right, rest)
	| otherwise      = error "Error paring formula"
	where
		(right, rest) = parseFormula tks

parseFormula (TkSyn LP : tks)
	| right /= Empty = (Imply left right, rest')
	| otherwise      = (left, [])
	where
		(left,  rest)  = parseFormula tks
		(right, rest') = parseFormula rest
parseFormula (TkSyn Curry : tks) = (right, rest)
	where
		(right, rest) = parseFormula tks
parseFormula (TkIdent ident : tks) = (Term ident, dropWhile (==TkSyn RP) tks)
parseFormula (TkSyn RP : tks) = (Empty, tks)
parseFormula tks = error $ "Error formula " ++ show tks