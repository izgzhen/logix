module PropParser where
import Tokenizer
import Data.List.Split

data Formula = Imply Formula Formula
			 | Term String
			 | Not Formula
			 | Empty deriving (Eq)

instance Show Formula where
	show (Term str) = str
	show (Not f) = "¬" ++ show f
	show (Imply front end) = "(" ++ show front ++ " → " ++ show end ++ ")"
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

------------------------------------------------------------------------

parseAxiom :: [Token] -> (String, [String], Formula)
parseAxiom (TkIdent name : TkSyn LP : tks) = (name, args, body)
	where
		(args, rest) = parseArgs $ tks
		(body, []) = parseFormula $ tail rest
parseAxiom _ = error "Error parsing axiom"
------------------------------------------------------------------------

parseArgs :: [Token] -> ([String], [Token])
parseArgs [] = ([],[])
parseArgs (TkIdent ident : TkSyn Comma : tks) = (ident : args, rest)
	where (args, rest) = parseArgs tks
parseArgs (TkIdent ident : TkSyn RP : tks) = ([ident], tks)
parseArgs tks = error $ "Error parsing args" ++ show tks

------------------------------------------------------------------------

parseFormulas :: [Token] -> [Formula]
parseFormulas tks = map (\(f, lts) -> if length lts == 0 then f
		else error "error parsing formulas") . map parseFormula $ splitOn [(TkSyn Comma)] tks

------------------------------------------------------------------------
-- A lot of boilerplate code here!!!

parseFormula :: [Token] -> (Formula, [Token])
parseFormula (TkSyn Neg : TkIdent ident: TkSyn Curry : tks) = (Imply (Not (Term ident)) right, tks')
	where (right, tks') = parseFormula tks
parseFormula (TkIdent ident : TkSyn Curry : tks) = (Imply (Term ident) right, tks')
	where (right, tks') = parseFormula tks

parseFormula (TkIdent ident : tks) = (Term ident, tks)
parseFormula (TkSyn Neg : TkIdent ident : tks) = (Not $ Term ident, tks)

parseFormula (TkSyn Neg : TkSyn LP : tks)
	| length rest > 0 && head rest == TkSyn Curry = (Imply (Not formula) formula'', tks'')
	| otherwise = (Not formula, rest)
	where
		(formula, (TkSyn RP : rest)) = parseFormula tks
		(formula'', tks'') = parseFormula $ tail rest

parseFormula (TkSyn LP : tks)
	| length rest > 0 && head rest == TkSyn Curry = (Imply formula formula'', tks'')
	| otherwise = (formula, rest)
	where
		(formula, (TkSyn RP : rest)) = parseFormula tks
		(formula'', tks'') = parseFormula $ tail rest