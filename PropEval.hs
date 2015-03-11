-- Evaluator of PropParser
module PropEval where
import PropParser
import Tokenizer
import Data.Char
import qualified Data.Map as M
import Control.Monad.State

data PropT = PropT [String] Formula deriving (Show, Eq)

type Context = M.Map String PropT

type Evaluator a = State Context a

lookUp :: String -> Evaluator Formula
lookUp name = do
    ctx <- get
    case M.lookup name ctx of
        Just (PropT args body) -> return body
        Nothing -> error $ "Undefined name " ++ name

addAxiom :: [Token] -> Evaluator ()
addAxiom tks = do
    ctx <- get
    put $ M.insert name (PropT args body) ctx
    return ()
    where
        (name, args, body) = parseAxiom tks

evaluate :: String -> Evaluator String
evaluate str
    | tk == TkKey Axiom = addAxiom tks >> return "Done"
    | tk == TkKey Check = do
        let (TkIdent name) = head tks
        formula <- lookUp name
        return $ show formula
    | otherwise = error "Error evaluate"
  where
    (tk:tks) = tokenize str

exampleStr1 = "Axiom L1(p) : p -> p"
exampleStr2 = "Axiom L1(p) : q -> p"

defaultContext = M.fromList [] :: M.Map String PropT


    -- print $ runState (evaluate $ tokenize exampleStr1) (M.fromList [])

    -- putStrLn $ show formula

-- evaluate :: Tree -> Evaluator Double

-- evaluate (SumNode op left right) = do
--     lft <- evaluate left
--     rgt <- evaluate right
--     case op of
--        Plus  -> return $ lft + rgt
--        Minus -> return $ lft - rgt

-- evaluate (ProdNode op left right) = do
--     lft <- evaluate left
--     rgt <- evaluate right
--     case op of
--        Times -> return $ lft * rgt
--        Div   -> return $ lft / rgt

-- evaluate (UnaryNode op tree) = do
--     x <- evaluate tree 
--     case op of
--        Plus  -> return x
--        Minus -> return (-x)

-- evaluate (NumNode x) = return x

-- evaluate (VarNode str) = lookUp str

-- evaluate (AssignNode str tree) = do
--     v <- evaluate tree
--     addSymbol str v
--     return v

-- expr = AssignNode "x" (ProdNode Times (VarNode "pi") 
--                                 (ProdNode Times (NumNode 4) (NumNode 6)))

-- main = print $ runState (evaluate expr) (M.fromList [("pi", pi)])