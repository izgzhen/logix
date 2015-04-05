module PropEval where

import PropParser
import Tokenizer
import Data.Char
import Data.List
import Data.Either
import qualified Data.Map as M
import Control.Monad
import Control.Monad.State


data PropT = PropT [String] Formula deriving (Show, Eq)
type SymbolContext = M.Map String PropT

type Evaluator a = State SymbolContext a

-- type EitherS = Either String

type PropContext = Maybe (String, PropT, Int)

maybePropName :: PropContext -> Maybe String
maybePropName (Just (name, _, _)) = Just name
maybePropName Nothing = Nothing

defaultPropCtx = Nothing

type PropState = EitherS PropContext

defaultContext = M.fromList [] :: M.Map String PropT

-------- Special Helpers --------------

(<|>) :: (Monad m) => (a -> EitherS b) -> (b -> c) -> (a -> m (EitherS c))

(<|>) f g = (\aVal -> return $ case f aVal of
    Right bVal -> Right (g bVal)
    Left err -> Left err)

(<||>) :: (Monad m) => (a -> EitherS b) -> (b -> m c) -> (a -> m (EitherS c))

(<||>) f g = (\aVal -> case f aVal of
        Right bVal -> do
            cVal <- g bVal
            return $ Right cVal
        Left err -> return $ Left err)

(<|||>) :: (Monad m) => (a -> m (EitherS b)) -> (b -> c) -> (a -> m (EitherS c))
(<|||>) f g = (\aVal -> do
    eBval <- f aVal
    case eBval of
        Right bVal -> return $ Right (g bVal)
        Left err -> return $ Left err)

(<||||>) :: (Monad m) => (a -> EitherS b) -> (b -> m (EitherS c)) -> (a -> m (EitherS c))
(<||||>) f g = (\aVal -> case f aVal of
        Right bVal -> do
            eCval <- g bVal
            case eCval of
                Right cVal -> return $ Right cVal
                Left err -> return $ Left err
        Left err -> return $ Left err)



plusOne :: PropContext -> PropContext
plusOne Nothing = Nothing
plusOne (Just (name, body, i)) = Just (name, body, i + 1)

-------- PURE FUNCTIONS ----------------

replaceIn :: (String, Formula) -> Formula -> Formula
replaceIn (slot, content) body = checkFormula body
    where
        checkFormula (Imply f1 f2) = (Imply (r' f1) (r' f2))
            where r' = replaceIn (slot, content)
        checkFormula formula@(Term str) = if slot == str then content else formula
        checkFormula Empty = Empty

unique = map (\(x:_) -> x) . group

extractArgs :: Formula -> [String]
extractArgs Empty = []
extractArgs (Imply f1 f2) = unique $ extractArgs f1 ++ extractArgs f2
extractArgs (Term str) = [str]

formulaToProp :: Formula -> PropT
formulaToProp f = PropT (extractArgs f) f

evalApply :: PropT -> [Formula] -> EitherS PropT
evalApply (PropT args body) formulas = do
    if length args /= length formulas
        then fail "length args is not equal to length formulas"
        else return $ (PropT (unique . concat $ map extractArgs formulas) (foldr replaceIn body (zip args formulas)))

mpApply :: Formula -> Formula -> EitherS Formula
mpApply f1@(Imply f1p f1c) f2@(Imply f2p f2c) = if f1 == f2p then return f2c
        else if f2 == f1p then return f1c
            else fail "mpApply error"
mpApply f1 (Imply f2p f2c) = if f1 == f2p then return f2c else fail "mpApply error"
mpApply (Imply f1p f1c) f2 = if f2 == f1p then return f1c else fail "mpApply error"
mpApply _ _ = fail "mpApply error"

-------- IMPURE FUNTIONS ---------------

lookUp :: String -> Evaluator (Maybe PropT)
lookUp name = do
    ctx <- get
    return $ M.lookup name ctx


addProp :: String -> PropT -> Evaluator ()
addProp name prop = do
    ctx <- get
    put $ M.insert name prop ctx


evalAxiom :: [Token] -> Evaluator (EitherS ())
evalAxiom = parseAxiom <||> (\(name, args, body) -> addProp name (PropT args body))

expandFormula :: Formula -> Evaluator Formula
expandFormula f@(Term ident) = do
    maybeProp <- lookUp ident
    return $ case maybeProp of
        Nothing -> f
        Just (PropT _ body) -> body
expandFormula (Not f') = liftM Not (expandFormula f')
expandFormula (Imply f1 f2) = liftM2 Imply (expandFormula f1) (expandFormula f2)


extractNamedFormula :: Token -> Evaluator (EitherS Formula)
extractNamedFormula (TkIdent ident) = do
            maybeProp <- lookUp ident
            return $ case maybeProp of
                Nothing -> Left "Not a defined name"
                Just (PropT _ body) -> Right body
extractNamedFormula _ = return $ Left "Only identifier can be extracted"

applyTheorem :: PropT -> [Formula] -> EitherS PropT
applyTheorem (PropT args body) formulas = do
    if length args /= length formulas then fail "illegal arguments number"
        else Right (PropT (unique . concat $ map extractArgs formulas) (foldr replaceIn body (zip args formulas)))

------ EVAL ------
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
evaluate (Just input) propCtx = case tokenize input of
    Right (tk:tks) -> do
        case tk of
            TkKey Axiom -> (evalAxiom <|||> (\() -> (Nothing, propCtx))) tks

            TkKey Check -> do
                str <- evalCheck tks
                return $ Right (Just str, propCtx)

            TkKey Theorem -> (parseAxiom <|> (\(name, args, body) -> (Nothing, Just (name, formulaToProp body, 0)))) tks

            TkTac Qed -> do
                case propCtx of
                    Nothing -> return $ fail "Not in PropContext!"
                    Just (name, body, i) -> do
                        (Just body') <- lookUp $ "S" ++ show (i - 1)
                        if body == body' then do
                            addProp name body
                            return $ Right (Just ("Proved: " ++ name ++ " " ++ show body), defaultPropCtx)
                            else return $ fail "Not proved yet!"

            TkTac Mp -> do
                    formulasE <- mapM extractNamedFormula $ filter (/= TkSyn Comma) tks
                    let formulas = rights formulasE
                    if length formulas == 2 then do
                            let f1 = head formulas
                            let f2 = head $ tail formulas
                            let retFE = mpApply f1 f2
                            case retFE of
                                Left err -> return $ fail "Unable to mp!"
                                Right retF -> do
                                    case propCtx of
                                        Nothing -> return $ fail "Not in PropContext!"
                                        Just (name, body, i) -> do
                                            let newName = "S" ++ show i
                                            let applied = formulaToProp retF
                                            addProp newName applied
                                            return $ Right (Just (newName ++ " " ++ show applied), plusOne propCtx)

                        else return $ fail "wrong MP arguments!"

            TkIdent ident -> do
                ret <- lookUp ident
                case ret of
                    Nothing -> return $ fail "No such identifier"
                    Just prop -> (parseFormulas <||||> (f prop)) tks
                    where
                        f :: PropT -> [Formula] -> Evaluator (EitherS (Maybe String, PropContext))
                        f prop formulas = do
                            tks' <- mapM expandFormula formulas
                            ((applyTheorem prop) <||> g) tks'
                                where
                                    g :: PropT -> Evaluator (Maybe String, PropContext)
                                    g applied = do
                                        case propCtx of
                                            Nothing -> return (Just "Not in PropContext!", propCtx)
                                            Just (name, body, i) -> do
                                                let newName = "S" ++ show i
                                                addProp newName applied
                                                return (Just (newName ++ " " ++ show applied), plusOne propCtx)


            _ -> return $ fail "Uable to evaluate"

    Left errorMsg -> return $ Left errorMsg