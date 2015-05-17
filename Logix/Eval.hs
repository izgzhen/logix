module Logix.Eval where

import Logix.Definition
import Logix.Parser
import Logix.Tokenizer
import Logix.Context
import Logix.Transform
import Logix.Utils
import Logix.Config

import Data.Char
import Data.List
import Data.Either
import qualified Data.Map as M
import Control.Monad
import Control.Monad.State

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
extractNamedFormula :: Int -> Evaluator (EitherS Formula)
extractNamedFormula i = do
    maybeProp <- lookUpRecord i
    return $ case maybeProp of
        Nothing -> Left "Not a defined name"
        Just step -> Right $ unStep step

-- Output Debug Info ---
debugInfo :: Show a => PropContext -> a -> Evaluator (Maybe String, PropContext)
debugInfo ctx x = return $ (Just $ "[DEBUG]: " ++ show x, ctx)

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
            return (Just (sShow i ++ " " ++ show applied ++ " with " ++ show strat), plusOne propCtx)

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


-- Main body of evalutation
-- FIXME: The input should not be strings, the REPL should make the interface in its part
--        So the silly function "unTokenize" will not appear in Logix.Sim module

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
                (Just (Step body' _ _)) <- lookUpRecord (i - 1)
                records <- gatherRecords
                if body == body' then do
                    addSymbol name body records
                    return $ Right (Just ("Proved: " ++ name ++ " " ++ show body), defaultPropCtx)
                    else return $ Left "Not proved yet!"

    TkTac Mp -> do
        let parsedIndex = parseIndex $ filter (/= TkSyn Comma) tks
        case parsedIndex of
            Right parsedIndex' -> do
                formulasE <- mapM extractNamedFormula parsedIndex'
                let formulas = rights formulasE
                if length formulas == 2 then do
                        let (f1 : f2 : _) = formulas
                        mpApply f1 f2 <||> (\retF -> addNewProp (formulaToProp retF) propCtx (Strategy MpRule parsedIndex'))
                    else return $ Left $ "wrong MP arguments -- " ++ show parsedIndex
            Left errMsg -> return $ Left errMsg

    TkIdent ident -> analyzeIdent ident <|||||> (\(strat, prop) -> 
        parseFormulas tks <||||> applyProp prop strat)
            where
                applyProp prop strat formulas = do
                    tks' <- mapM expandFormula formulas
                    ((applyTheorem prop tks') <||> (\p -> addNewProp p propCtx strat))
                analyzeIdent :: String -> Evaluator (EitherS (Strategy, PropT))
                analyzeIdent ident = do
                    mProp <- lookUpSymbol ident
                    case mProp of
                        Just prop -> return $ case M.lookup ident skMap' of
                                Just sk -> return (Strategy sk [], prop)
                                Nothing -> fail $ "no such strategy: " ++ show ident
                        Nothing -> return $ fail $ "no such identifier: " ++ show ident

    _ -> return $ Left "Uable to evaluate"