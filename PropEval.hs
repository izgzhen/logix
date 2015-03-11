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

defaultContext = M.fromList [] :: M.Map String PropT

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

addTheorem :: [Token] -> Evaluator (PropT, String) -- Return the name of theorem
addTheorem tks = do
    let theorem@(name, args, body) = parseAxiom tks
    return (PropT args body, name)

applyTheorem :: Token -> [Token] -> Evaluator String
applyTheorem = undefined
-- applyTheorem (TkIdent name) tks = do
-- applyTheorem _ _ = error "Error applying"

evaluate :: String -> Maybe PropT-> Evaluator (Maybe String, Maybe String, Maybe PropT)
evaluate str maybeProp
    | tk == TkKey Axiom = addAxiom tks >> return (Nothing, Nothing, Nothing)
    | tk == TkKey Check = do
        let (TkIdent name) = head tks
        formula <- lookUp name
        return (Just $ show formula, Nothing, Nothing)
    | tk == TkKey Theorem = do
        (propCtx, name) <- addTheorem tks
        return (Nothing, Just name, Just propCtx)
    | isIndent tk = do
        res <- applyTheorem tk tks
        return (Just res, Nothing, Nothing)
    | otherwise = error "Error evaluate"
  where
    (tk:tks) = tokenize str

isIndent (TkIdent _) = True
isIndent _ = False

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