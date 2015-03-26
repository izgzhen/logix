-- Evaluator of PropParser
module PropEval where
import PropParser
import Tokenizer
import Data.Char
import qualified Data.Map as M
import Control.Monad.State
import Data.List
import Control.Monad

data PropT = PropT [String] Formula deriving (Show, Eq)
type Context = M.Map String PropT
type Evaluator a = State Context a

defaultContext = M.fromList [] :: M.Map String PropT

lookUp :: String -> Evaluator (Maybe PropT)
lookUp name = do
    ctx <- get
    return $ M.lookup name ctx

addAxiom :: [Token] -> Evaluator ()
addAxiom tks = addProp name (PropT args body)
    where (name, args, body) = parseAxiom tks

newTheorem :: [Token] -> Evaluator (PropT, String) -- Return the name of theorem
newTheorem tks = do
    let theorem@(name, args, body) = parseAxiom tks
    return (PropT args body, name)

addProp :: String -> PropT -> Evaluator ()
addProp name prop = do
    ctx <- get
    put $ M.insert name prop ctx

{-
    apply theorem involves:
        get the theorem
        parse the arguments
        add the new theorem
        return the new theorem as a string
-}

replaceIn :: (String, Formula) -> Formula -> Formula
replaceIn (slot, content) body = checkFormula body
    where
        checkFormula (Imply f1 f2) = (Imply (r' f1) (r' f2))
            where r' = replaceIn (slot, content)
        checkFormula formula@(Term str)    = if slot == str then content else formula
        checkFormula Empty         = Empty

unique = map (\(x:_) -> x) . group

extractArgs :: Formula -> [String]
extractArgs Empty = []
extractArgs (Imply f1 f2) = extractArgs f1 ++ extractArgs f2
extractArgs (Term str) = [str]

applyTheorem :: PropT -> [Formula] -> Maybe PropT
applyTheorem (PropT args body) formulas = do
    if length args /= length formulas
        then Nothing
        else Just (PropT (unique . concat $ map (unique . extractArgs) formulas) (foldr replaceIn body (zip args formulas)))

expandFormula :: Formula -> Evaluator Formula
expandFormula f@(Term ident) = do
    maybeProp <- lookUp ident
    return $ case maybeProp of
        Nothing -> f
        Just (PropT _ body) -> body
expandFormula (Not f') = liftM Not (expandFormula f')
expandFormula (Imply f1 f2) = liftM2 Imply (expandFormula f1) (expandFormula f2)


evaluate :: String -> Maybe (PropT, Int) -> Evaluator (Maybe String, Maybe String, Maybe (PropT, Int))
evaluate str maybePropCtx
    | tk == TkKey Axiom = addAxiom tks >> return (Nothing, Nothing, maybePropCtx)

    | tk == TkKey Check = do
        let (TkIdent name) = head tks
        (Just (PropT _ formula)) <- lookUp name
        return (Just $ show formula, Nothing, maybePropCtx)

    | tk == TkKey Theorem = do
        (propCtx, name) <- newTheorem tks
        return (Nothing, Just name, Just (propCtx, 0))

    | (TkIdent ident) <- tk = do
        (Just theorem) <- lookUp ident
        tks' <- mapM expandFormula $ parseFormulas tks
        case applyTheorem theorem tks' of
            Just applied -> do
                let (Just (prop, i)) = maybePropCtx
                let newName = "S" ++ show i
                addProp newName applied
                return (Just (newName ++ " " ++ show applied), Nothing, Just (prop, i + 1))
            Nothing -> do
                return (Just ("Wrong applying!"), Nothing, maybePropCtx)
    | otherwise = error "Error evaluate"
  where
    (tk:tks) = tokenize str

-- isIndent (TkIdent _) = True
-- isIndent _ = False

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