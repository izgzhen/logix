module Logix.PropEval where

import Logix.PropParser
import Logix.Tokenizer
import Data.Char
import Data.List
import Data.Either
import qualified Data.Map as M
import Control.Monad
import Control.Monad.State


data PropT = PropT [String] Formula deriving (Show, Eq) -- arguments and body

data Deriving = Assume | Negfront deriving (Show, Eq)
data Strategy = StTac Tactic | StDer Deriving deriving (Show, Eq)

getStrat :: String -> Strategy
getStrat "assum" = StDer Assume
getStrat "negfront" = StDer Negfront
getStrat "L1" = StTac L1
getStrat "L2" = StTac L2
getStrat "L3" = StTac L3

type SymbolContext = M.Map String PropT -- Map name to proposition

type StepRecord = M.Map Int (PropT, Strategy)

type Evaluator a = State (SymbolContext, StepRecord) a -- Context Wrapper

type PropContext = Maybe (String, PropT, Int) -- Proposition information which is in being proved

maybePropName :: PropContext -> Maybe String -- A handy destructor
maybePropName (Just (name, _, _)) = Just name
maybePropName Nothing = Nothing

-- Default Settings
defaultPropCtx = Nothing
defaultContext = (M.fromList [] :: M.Map String PropT, M.fromList [] :: M.Map Int (PropT, Strategy))

-- Helpers for Error Handling, just the type signatures will be meaningful

(<|>) :: (Monad m) => EitherS b -> (b -> c) -> m (EitherS c)
(<||>) :: (Monad m) => EitherS b -> (b -> m c) -> m (EitherS c)
(<|||>) :: (Monad m) => m (EitherS b) -> (b -> c) -> m (EitherS c)
(<||||>) :: (Monad m) => EitherS b -> (b -> m (EitherS c)) -> m (EitherS c)

(<|>) val g = return $ case val of
    Right bVal -> Right (g bVal)
    Left err -> Left err

(<||>) val g = case val of
        Right bVal -> liftM Right $ g bVal
        Left err -> return $ Left err

(<|||>) val g = val >>= (flip (<|>) g)

(<||||>) val g = case val of
        Right bVal -> (g bVal) <|||> id
        Left err -> return $ Left err

-- Handy function for in-proving context advancing
plusOne :: PropContext -> PropContext
plusOne Nothing = Nothing
plusOne (Just (name, body, i)) = Just (name, body, i + 1)

-- Just for readability
unique = map (\(x:_) -> x) . group . sort

-------- PURE FUNCTIONS ----------------

-- Replace matched slot with content in a certain body
replaceIn :: (String, Formula) -> Formula -> Formula
replaceIn (slot, content) body = checkFormula body
    where
        checkFormula (Imply f1 f2) = (Imply (r' f1) (r' f2))
        checkFormula formula@(Term str) = if slot == str then content else formula
        checkFormula Empty = Empty
        checkFormula (Not f) = Not $ r' f
        r' = replaceIn (slot, content)

-- Extract all appeared arguments from a formula body
extractArgs :: Formula -> [String]
extractArgs Empty = []
extractArgs (Imply f1 f2) = unique $ extractArgs f1 ++ extractArgs f2
extractArgs (Term str) = [str]
extractArgs (Not f) = unique $ extractArgs f

-- Transform a formula to a proposition
formulaToProp :: Formula -> PropT
formulaToProp f = PropT (extractArgs f) f

-- Apply a proposition by substituting the formal arguments into real arguments
evalApply :: PropT -> [Formula] -> EitherS PropT
evalApply (PropT args body) formulas = do
    if length args /= length formulas
        then Left "length args is not equal to length formulas"
        else return $ (PropT (unique . concat $ map extractArgs formulas) (foldr replaceIn body (zip args formulas)))

-- MP rule -- (a -> b) -> (b -> c) -> (a -> c)
mpApply :: Formula -> Formula -> EitherS Formula
mpApply f1@(Imply f1p f1c) f2@(Imply f2p f2c) = if f1 == f2p then return f2c
        else if f2 == f1p then return f1c else mpErr
mpApply f1 (Imply f2p f2c) = if f1 == f2p then return f2c else mpErr
mpApply (Imply f1p f1c) f2 = if f2 == f1p then return f1c else mpErr
mpApply _ _ = mpErr

mpErr = Left "MP Rule Applying Incorrect"

-------- IMPURE FUNTIONS ---------------

-- Lookup a proposition by its name
lookUpSymbol :: String -> Evaluator (Maybe PropT)
lookUpSymbol name = do
    (sCtx, sRecord) <- get
    return $ M.lookup name sCtx

lookUpRecordByString :: String -> Evaluator (Maybe PropT)
lookUpRecordByString ('S':indexIdent) = do
    (sCtx, sRecord) <- get
    case reads indexIdent :: [(Int, String)] of
        (num, ""):[] -> do
            return $ liftM fst $ M.lookup num sRecord
        _ -> return Nothing
lookUpRecordByString _ = return Nothing

lookUpRecord :: Int -> Evaluator (Maybe (PropT, Strategy))
lookUpRecord index = do
    (sCtx, sRecord) <- get
    return $ M.lookup index sRecord

lookUp :: String -> Evaluator (Maybe PropT)
lookUp name = liftM msum $ sequence [lookUpSymbol name, lookUpRecordByString name]

-- Add a proposition by its name
addSymbol :: String -> PropT -> Evaluator ()
addSymbol name prop = do
    (sCtx, sRecord) <- get
    put $ (M.insert name prop sCtx, sRecord)

addRecord :: Int -> PropT -> Strategy -> Evaluator ()
addRecord index prop strat = do
    (sCtx, sRecord) <- get
    put $ (sCtx, M.insert index (prop, strat) sRecord)

-- Add an axiom to the context
addAxiom :: [Token] -> Evaluator (EitherS ())
addAxiom = \tks -> parseAxiom tks <||||> (\(name, args, body) -> do
    if sort (extractArgs body) == sort args then do
            addSymbol name (PropT args body)
            return $ Right ()
        else return $ Left "arguments not matching in axiom")

-- Expand the formula based on current context
expandFormula :: Formula -> Evaluator Formula
expandFormula f@(Term ident) = do
    maybeProp <- lookUpRecordByString ident
    return $ case maybeProp of
        Nothing -> f
        Just (PropT _ body) -> body
expandFormula (Not f') = liftM Not (expandFormula f')
expandFormula (Imply f1 f2) = liftM2 Imply (expandFormula f1) (expandFormula f2)

-- Extract the body of named proposition from context by a token, handling all exceptions
extractNamedFormula :: Token -> Evaluator (EitherS Formula)
extractNamedFormula (TkIdent ident) = do
            maybeProp <- lookUpRecordByString ident
            return $ case maybeProp of
                Nothing -> Left "Not a defined name"
                Just (PropT _ body) -> Right body
extractNamedFormula _ = return $ Left "Only identifier can be extracted"

-- apply theorem to a set of formulas, getting the generated body
applyTheorem :: PropT -> [Formula] -> EitherS PropT
applyTheorem (PropT args body) formulas = do
    if length args /= length formulas then Left "illegal arguments number"
        else Right (PropT (unique . concat $ map extractArgs formulas) (foldr replaceIn body (zip args formulas)))

-- Add a step during proving
addNewProp :: PropT -> PropContext -> Strategy -> Evaluator (Maybe String, PropContext)
addNewProp applied propCtx strat = do
    case propCtx of
        Nothing -> return (Just "Not in PropContext!", propCtx)
        Just (name, body, i) -> do
            addRecord i applied strat
            return (Just ("S" ++ show i ++ " " ++ show applied), plusOne propCtx)

-- "Check" Command
evalCheck :: [Token] -> Evaluator String
evalCheck [] = return ""
evalCheck (tk:tks) = do
    restStr <- evalCheck tks
    thisStr <- case tk of
        TkIdent name -> do
            mProp <- lookUp name
            return $ case mProp of
                Just (PropT _ formula) -> show formula
                Nothing -> "No such identifier " ++ name
        _ -> return $ "illegal identifier: " ++ show tk
    return $ thisStr ++ restStr

evaluate :: Maybe String -> PropContext -> Evaluator (EitherS (Maybe String, PropContext))
evaluate Nothing propCtx = return (Right (Nothing, propCtx))
evaluate (Just input) propCtx = tokenize input <||||> \(tk:tks) -> case tk of
    TkKey Axiom -> (addAxiom tks <|||> (\() -> (Nothing, propCtx)))

    TkKey Check -> do
        str <- evalCheck tks
        return $ Right (Just str, propCtx)

    TkKey Theorem -> (parseAxiom tks <|> (\(name, args, body) -> (Nothing, Just (name, formulaToProp body, 0))))

    TkTac Qed -> do
        case propCtx of
            Nothing -> return $ Left "Not in PropContext!"
            Just (name, body, i) -> do
                (Just (body', _)) <- lookUpRecord (i - 1)
                if body == body' then do
                    addSymbol name body
                    return $ Right (Just ("Proved: " ++ name ++ " " ++ show body), defaultPropCtx)
                    else return $ Left "Not proved yet!"

    TkTac Mp -> do
            formulasE <- mapM extractNamedFormula $ filter (/= TkSyn Comma) tks
            let formulas = rights formulasE
            if length formulas == 2 then do
                    let (f1 : f2 : _) = formulas
                    mpApply f1 f2 <||> (\retF -> addNewProp (formulaToProp retF) propCtx $ StTac Mp)
                else return $ Left "wrong MP arguments!"

    TkIdent ident -> do
        ret <- lookUpSymbol ident
        case ret of
            Nothing -> return $ Left $ "No such identifier '" ++ ident ++ "'"
            Just prop -> (parseFormulas tks <||||> (applyProp prop))
            where
                strat = getStrat ident
                applyProp prop formulas = do
                    tks' <- mapM expandFormula formulas
                    ((applyTheorem prop tks') <||> (\p -> addNewProp p propCtx strat))

    _ -> return $ Left "Uable to evaluate"