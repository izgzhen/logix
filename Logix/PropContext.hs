module Logix.PropContext where

import Logix.PropParser
import Logix.Tokenizer
import Logix.Unwrap
import Logix.Utils
import Data.Char
import Data.List
import Data.Either
import qualified Data.Map as M
import Control.Monad
import Control.Monad.State

data PropT = PropT [String] Formula deriving (Show, Eq) -- arguments and body

data StrategyKind = MpRule | Negfront | L1 | L2 | L3 deriving (Show)
data StrategyInstance = Strategy StrategyKind [Int] -- Name and its arguments

type Step = (PropT, StrategyInstance)
type SymbolContext = M.Map String (PropT, [Step]) -- Map name to proposition
type StepRecord = M.Map Int Step -- Record steps during proving
type Evaluator a = State (SymbolContext, StepRecord) a -- Context Wrapper
type PropContext = Maybe (String, PropT, Int) -- Proposition information which is in being proved

maybePropName :: PropContext -> Maybe String -- A handy destructor
maybePropName (Just (name, _, _)) = Just name
maybePropName Nothing = Nothing

-- Default Settings
defaultPropCtx = Nothing
preloadedAxioms = M.fromList [] :: SymbolContext
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

lookUpRecordByString :: String -> Evaluator (Maybe PropT)
lookUpRecordByString ('S':indexIdent) = do
    (sCtx, sRecord) <- get
    case reads indexIdent :: [(Int, String)] of
        (num, ""):[] -> do
            return $ liftM fst $ M.lookup num sRecord
        _ -> return Nothing
lookUpRecordByString _ = return Nothing

lookUpRecord :: Int -> Evaluator (Maybe Step)
lookUpRecord index = do
    (sCtx, sRecord) <- get
    return $ M.lookup index sRecord

lookUp :: String -> Evaluator (Maybe PropT)
lookUp name = liftM msum $ sequence [lookUpSymbol name, lookUpRecordByString name]

-- Add a proposition by its name
addSymbol :: String -> PropT -> [Step] -> Evaluator ()
addSymbol name prop steps = do
    (sCtx, sRecord) <- get
    put $ (M.insert name (prop, steps) sCtx, sRecord)

addRecord :: Int -> PropT -> StrategyInstance -> Evaluator ()
addRecord index prop strat = do
    (sCtx, sRecord) <- get
    put $ (sCtx, M.insert index (prop, strat) sRecord)

