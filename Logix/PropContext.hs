module Logix.PropContext where

import Logix.PropDefs
import Logix.PropParser
import Logix.Tokenizer
import Logix.PropTransform
import Logix.Utils
import Data.Char
import Data.List
import Data.Either
import qualified Data.Map as M
import Control.Monad
import Control.Monad.State

maybePropName :: PropContext -> Maybe String -- A handy destructor
maybePropName (Just (name, _, _)) = Just name
maybePropName Nothing = Nothing

preloadedAxioms = M.fromList [
      ("L1", (strToProp "p -> (q -> p)", []))
    , ("L2", (strToProp "(p -> (q -> r)) -> ((p -> q) -> (p -> r))", []))
    , ("L3", (strToProp "(p -> q) -> (!q -> !p)", []))
    ] :: SymbolContext

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

addRecord :: Int -> PropT -> Strategy -> Evaluator ()
addRecord index prop strat = do
    (sCtx, sRecord) <- get
    put $ (sCtx, M.insert index (Step prop strat index) sRecord)

gatherRecords :: Evaluator [Step]
gatherRecords = do
    (_, sRecord) <- get
    return $ map snd $ M.toAscList sRecord

-- Add an axiom to the context
addAxiom :: [Token] -> Evaluator (EitherS ())
addAxiom = \tks -> parseAxiom tks <||||> (\(name, args, body) -> do
    if sort (extractArgs body) == sort args then do
            addSymbol name (PropT args body) []
            return $ Right ()
        else return $ Left "arguments not matching in axiom")

