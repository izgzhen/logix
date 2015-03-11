module Parser where
import Tokenizer
import qualified Data.Map as Map

type Block = [(Int, String)]    -- Indentation Level and the line's content
type TBlock = [(Int, [Token])]

blocker :: Block -> [ Block ] -- From a whole "block"
blocker [] = []
blocker lns = b : blocker rest
    where
        headLn = head lns
        (b', rest) = span (\ln -> fst ln /= 0) $ tail lns
        b = headLn:b'

process :: String -> [ Block ]
process txt = blocker . map stripper $ filter (/= "") . lines $ txt

constructBlock :: Block -> TBlock
constructBlock block = map (\(i, str) -> (i, tokenize str)) block

readTokens filename = do
    text <- readFile filename
    return $ map constructBlock $ process text

stripper :: String -> (Int, String)
stripper line = (tabs, stripped)
    where
        stripped = dropWhile (=='\t') line
        tabs = length line - length stripped

parseBlock :: TBlock -> String
parseBlock blk@(x:xs) = parse blk
	where
		(_, (TkKey blockIdent):_) = x
		parse = case blockIdent of
			Type	-> show . parseType
			Ind		-> show . parseInd
			Def		-> show . parseDef
			Prop	-> show . parseProp
			Prove	-> show . parseProve
			Check   -> show . parseCheck
parseBlock [] = ""

type Expr = [ Term ]
data Term = SubExpr Expr | Factor String deriving (Show, Eq)

data TypeSpec = TypeSpec {
	typename    :: String,
	fathertype	:: String,
	deadEnds	:: [String],
	inductive	:: [(String, Expr)]
} deriving (Show, Eq)

parseExpr :: [Token] -> Expr
parseExpr [] = []
parseExpr (TkSyn LP : ts)
	| length body' > 1 = (SubExpr . parseExpr . init) body : parseExpr rest
	where
		(rest', body') = span (/= TkSyn RP) $ reverse ts
		rest = reverse rest'
		body = reverse body'

parseExpr (TkIdent ident : ts) = Factor ident : parseExpr ts
parseExpr xs = error $ "Illegal Expr" ++ show xs


parseTypeHead :: [Token] -> (String, String)
parseTypeHead ((TkIdent name):(TkSyn Of):(TkIdent faname):[]) = (name, faname)
parseTypeHead xs = error $ "Error: parsing type head at " ++ show xs

parseType :: TBlock -> TypeSpec
parseType ((_, ((TkKey Type):ts)):xs) = TypeSpec ident fa ends inds
	where
		(ident, fa) = parseTypeHead ts
		ends = map (\(_, _:(TkIdent ident):[]) -> ident) . filter (\(_, tks) -> length tks == 2) $ xs
		inds = map (\(_, tks@(_:(TkIdent ident):_)) -> (ident, parseExpr (tail (tail tks)))). filter (\(_, tks) -> length tks > 2) $ xs

data IndSpec  = IndSpec {
	indname 	:: String,
	indtype 	:: [String], 	-- Application Type
	rules  		:: [(Expr, Expr)]	-- Argument Pattern and Expansion
} deriving (Show, Eq)

parseCurry :: [Token] -> [String]
parseCurry [] = []
parseCurry (TkIdent n : []) = [n]
parseCurry (TkIdent n : TkSyn Curry : xs) = n : parseCurry xs
parseCurry _ = error "Error: parsing Curry"

parseIndHead :: (Int, [Token]) -> (String, [String])
parseIndHead (_, _:(TkIdent name):(TkSyn Of):xs) = (name, parseCurry xs)
parseIndHead (_, _) = error "Error: parsing ind head"

parseIndRule :: [Token] -> (Expr, Expr)
parseIndRule tks
	| length first /= 0 && length second > 1 = (parseExpr $ tail first, parseExpr $ tail second)
	| otherwise = error "Error: parsing ind rule"
	where
		(first, second) = span (/= TkSyn Eq) tks

parseInd :: TBlock -> IndSpec
parseInd (x:xs) = IndSpec n t rs
	where
		(n, t) = parseIndHead x
		rs = map parseIndRule . tail . 	map (\(_, t) -> t) $ xs

data DefSpec = DefSpec {
	defname :: String,
	defbody :: Expr
} deriving (Show, Eq)

parseDef :: TBlock -> DefSpec
parseDef ((_, tks):[])
	| length first == 1 && length second > 1 = DefSpec n $ parseExpr $ tail second 
	| otherwise = error "Error: parsing ind rule"
	where
		(first@((TkIdent n):xs), second) = span (/= TkSyn Eq) (tail tks) -- Drop the guard

type OfStatement = (String, String)
type EqStatement = (Expr, Expr)

data Statement = Sof OfStatement
			   | Seq EqStatement
			   deriving (Show, Eq)

data PropSpec = PropSpec {
	propname    :: String,
	conditions  :: [Statement],
	conclusions :: [Statement]
} deriving (Show, Eq)

parseStatment :: [Token] -> [Statement]
parseStatment [] = []
parseStatment tks
	| tks !! 1 == TkSyn Of = Sof (parseTypeHead s) : parseStatment ss
	| otherwise = Seq (parseIndRule s) : parseStatment ss
	where
		(s, ss') = span (/= TkSyn Comma) tks
		ss = if length ss' > 1 then tail ss' else []

parseProp :: TBlock -> PropSpec
parseProp (x:y:z:[]) = PropSpec n cds ccs
	where
		(_, _:(TkIdent n):[]) = x
		(_, (TkKey If):ys) = y
		cds = parseStatment ys
		(_, (TkKey Then):zs) = z
		ccs = parseStatment zs

parseProp _ = error "Error: parsing prop"


data ProveSpec = ProveSpec {
	target :: String,
	steps  :: [(Tactic, [String])]
} deriving (Show, Eq)

parseProve :: TBlock -> ProveSpec
parseProve ((_, _:(TkIdent n):[]):xs) = ProveSpec n ss
	where
		ss = map (\(_, TkTac tac : body) -> (tac, map (\(TkIdent n) -> n) body)) xs

data CheckSpec = CheckSpec Expr deriving(Show, Eq)

parseCheck :: TBlock -> CheckSpec
parseCheck ((_, (TkKey Check):ts):[]) = CheckSpec $ parseExpr ts