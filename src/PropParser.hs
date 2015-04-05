module PropParser where

import Tokenizer
import Data.List.Split
import Data.Maybe
import Data.Either

data Formula = Imply Formula Formula
			 | Term String
			 | Not Formula
			 | Empty deriving (Eq)

instance Show Formula where
	show (Term str) = str
	show (Not f) = "¬" ++ show f
	show (Imply front end) = "(" ++ show front ++ " → " ++ show end ++ ")"
	show Empty = ""

parseLine :: String -> EitherS String -- Input and Response
parseLine ln = do
	tks <- tokenize ln
	case (head tks) of
		TkKey Axiom -> return . show . parseAxiom $ tail tks
		_           -> fail "illegal parsing line"

parseAxiom :: [Token] -> EitherS (String, [String], Formula)
parseAxiom ts@(TkIdent name : TkSyn LP : tks) = do
	(args, rest) <- parseArgs tks
	ret <- parseFormula (tail rest)
	return $ let (body, []) = ret in (name, args, body)
parseAxiom ts = fail $ "Error in parsing " ++ show ts


parseArgs :: [Token] -> EitherS ([String], [Token])
parseArgs [] = return ([],[])
parseArgs (TkIdent ident : TkSyn Comma : tks) = do
	(args, rest) <- parseArgs tks
	return (ident : args, rest)
parseArgs (TkIdent ident : TkSyn RP : tks) = return ([ident], tks)
parseArgs tks = fail $ "Error parsing args" ++ show tks


parseFormulas :: [Token] -> EitherS [Formula]
parseFormulas tks = do
	mapM (isValidFormula . parseFormula) $ splitOn [(TkSyn Comma)] tks
	where isValidFormula x = do
		(f, lts) <- x
		if length lts == 0 then return f
				else fail "error parsing formula"


parseFormula :: [Token] -> EitherS (Formula, [Token])
parseFormula (TkSyn Neg : TkIdent ident : TkSyn Curry : tks) = do
	(right, tks') <- parseFormula tks
	return (Imply (Not (Term ident)) right, tks')

parseFormula (TkIdent ident : TkSyn Curry : tks) = do
	(right, tks') <- parseFormula tks
	return (Imply (Term ident) right, tks')

parseFormula (TkIdent ident : tks) = return (Term ident, tks)
parseFormula (TkSyn Neg : TkIdent ident : tks) = return (Not $ Term ident, tks)
parseFormula (TkSyn Neg : TkSyn LP : tks) = parseFormula' tks Not
parseFormula (TkSyn LP : tks) = parseFormula' tks id
parseFormula (TkSyn Ax : tks') = parseFormula tks' -- A little hack
parseFormula _ = fail "parseFormula Error"

parseFormula' :: [Token] -> (Formula -> Formula) -> EitherS (Formula, [Token])
parseFormula' tks f = do
	(formula, (TkSyn RP : rest)) <- parseFormula tks
	(formula'', tks'') <- parseFormula $ tail rest
	return $ if length rest > 0 && head rest == TkSyn Curry then
		(Imply (f formula) formula'', tks'')
		else (f formula, rest)