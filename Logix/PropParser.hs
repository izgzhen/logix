module Logix.PropParser where
import Logix.PropDefs
import Logix.Tokenizer
import Data.List.Split
import Data.Maybe
import Data.Either

parseLine :: String -> EitherS String -- Input and Response
parseLine ln = do
	tks <- tokenize ln
	case (head tks) of
		TkKey Axiom -> return . show . parseAxiom $ tail tks
		_           -> Left "illegal parsing line"

parseAxiom :: [Token] -> EitherS (String, [String], Formula)
parseAxiom ts@(TkIdent name : TkSyn LP : tks) = do
	(args, rest) <- parseArgs tks
	ret <- parseFormula (tail rest)
	return $ let (body, []) = ret in (name, args, body)
parseAxiom ts = Left $ "Error in parsing Axiom: " ++ show ts


parseArgs :: [Token] -> EitherS ([String], [Token])
parseArgs [] = return ([],[])
parseArgs (TkIdent ident : TkSyn Comma : tks) = do
	(args, rest) <- parseArgs tks
	return (ident : args, rest)
parseArgs (TkIdent ident : TkSyn RP : tks) = return ([ident], tks)
parseArgs tks = Left $ "Error parsing args" ++ show tks

parseIndex :: [Token] -> EitherS [Int]
parseIndex [] = Right []
parseIndex (TkIdent ('S':indexStr) : fs) = case reads indexStr :: [(Int, String)] of
	(num, ""):[] -> do
		pfs <- parseIndex fs
		return  (num : pfs)
	_ -> fail  "Not proper numerical arguments"
parseIndex x = fail $ "illegal index with " ++ show x

parseFormulas :: [Token] -> EitherS [Formula]
parseFormulas tks = do
	mapM (isValidFormula . parseFormula) $ splitOn [(TkSyn Comma)] tks
	where isValidFormula x = do
		(f, lts) <- x
		if length lts == 0 then return f
				else Left "error parsing formula"


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
parseFormula _ = Left "parseFormula Error"

parseFormula' :: [Token] -> (Formula -> Formula) -> EitherS (Formula, [Token])
parseFormula' tks f = do
	(formula, (TkSyn RP : rest)) <- parseFormula tks
	if length rest > 0 && head rest == TkSyn Curry then do
		(formula'', tks'') <- parseFormula $ tail rest
		return (Imply (f formula) formula'', tks'')
		else return (f formula, rest)