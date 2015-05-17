{-
    Provides context-related operations
-}

module Logix.Context where

import Logix.Definition
import Logix.Parser
import Logix.Tokenizer
import Logix.Transform
import Logix.Config
import Logix.Utils
import Data.Char
import Data.List
import Data.Either
import qualified Data.Map as M
import Control.Monad
import Control.Monad.State

-- Only a handy destructor
maybePropName :: PropContext -> Maybe String
maybePropName (Just (name, _, _)) = Just name
maybePropName Nothing = Nothing

-- Default Settings
defaultPropCtx = Nothing
defaultContext = (preloadedAxioms, M.fromList [] :: StepRecord)

-- Handy function for in-proving context advancing
plusOne :: PropContext -> PropContext
plusOne Nothing = Nothing
plusOne (Just (name, body, i)) = Just (name, body, i + 1)

-- Lookup a proposition by its name
lookUpSymbol :: String -> Evaluator (Maybe PropT)
lookUpSymbol name = do
    (sCtx, sRecord) <- get
    return $ liftM fst $ M.lookup name sCtx

-- Lookup a proposition during steps with string name
lookUpRecordByString :: String -> Evaluator (Maybe PropT)
lookUpRecordByString ('S':indexIdent) = do
    (sCtx, sRecord) <- get
    case reads indexIdent :: [(Int, String)] of
        (num, ""):[] -> do
                let mStep = M.lookup num sRecord
                case mStep of
                    Just _ -> return Nothing
                    Nothing -> return Nothing
        _ -> return Nothing
lookUpRecordByString _ = return Nothing

-- Lookup with index
lookUpRecord :: Index -> Evaluator (Maybe Step)
lookUpRecord index = do
    (sCtx, sRecord) <- get
    return $ M.lookup index sRecord

-- General loopUp
lookUp :: String -> Evaluator (Maybe PropT)
lookUp name = liftM msum $ sequence [lookUpSymbol name, lookUpRecordByString name]

-- Add a proposition by its name
addSymbol :: String -> PropT -> [Step] -> Evaluator ()
addSymbol name prop steps = do
    (sCtx, sRecord) <- get
    put $ (M.insert name (prop, steps) sCtx, sRecord)

-- Add a record by its index
addRecord :: Index -> PropT -> Strategy -> Evaluator ()
addRecord index prop strat = do
    (sCtx, sRecord) <- get
    put $ (sCtx, M.insert index (Step prop strat index) sRecord)

-- Add an axiom to the context
addAxiom :: [Token] -> Evaluator (EitherS ())
addAxiom = \tks -> parseAxiom tks <||||> (\(name, args, body) -> do
    if sort (extractArgs body) == sort args then do
            addSymbol name (PropT args body) []
            return $ Right ()
        else return $ Left "arguments not matching in axiom")

-- Gather records from current context
gatherRecords :: Evaluator [Step]
gatherRecords = do
    (_, sRecord) <- get
    return $ map snd $ M.toAscList sRecord
