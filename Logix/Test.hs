module Logix.Test where
import Logix.PropParser
import Logix.PropDefs
import Logix.Tokenizer
import Logix.PropContext
import Logix.PropTransform
import Control.Monad.State
import Logix.Sim
import qualified Data.Map as M

-- import Logix.PropTransform
-- import Logix.Utils

--------- TEST HELPERS -----------

-- deriveSteps :: [String] -> [(StrategyKind, [Int])] -> [Step] -> SimT m ()
-- deriveSteps args tactics = deriveLoop tactics defaultPropCtx defaultContext
-- 	where
-- 		deriveLoop :: MonadException m => PropContext -> (SymbolContext, StepRecord) -> InputT m ()
-- 		deriveLoop propCtx ctx = do

-- negfrontProof = genProof Negfront ["p", "q"]
-- 	[ ("(!p -> !q) -> (q -> p)", L3, [])
-- 	, ("((!p -> !q) -> (q -> p)) -> (!q -> ", L2)
-- 	, ("p -> (p -> q)", L3) ]

-- genProof' :: StrategyKind -> [String] -> [(String, StrategyKind)] -> M.Map StrategyKind Proof
-- genProof' sk args steps = M.fromList [ (sk, Proof (PropT args $ (unStep . last) steps') steps') ]
-- 	where
-- 		steps' = map genStep' $ zip steps [0..]

-- genProof :: StrategyKind -> [String] -> [(String, StrategyKind, [Int])] -> M.Map StrategyKind Proof
-- genProof sk args steps = M.fromList [ (sk, Proof (PropT args $ (unStep . last) steps') steps') ]
-- 	where
-- 		steps' = map genStep steps

-- genStep' :: ((String, StrategyKind), Int) -> Step
-- genStep' ((str, sk), i) = (formulaToProp formula, Strategy sk [i])
-- 	where
-- 		Right (formula, _) = tokenize str >>= parseFormula

-- genStep :: (String, StrategyKind, [Int]) -> Step
-- genStep (str, sk, is) = (strToProp str, Strategy sk is)

-- argsToTest = map Term ["a", "b"]

-- stepsToTest :: [(String, StrategyKind)]
-- stepsToTest = [ ("p", Assume)
-- 	, ("p -> q", MpRule)
-- 	, ("p -> (p -> q)", Negfront)]

stepsToTest :: [String]
stepsToTest = [
	  "L1 p, (p -> p)"
	, "L2 p, (p -> p), p"
	, "mp S0, S1"
	, "L1 p, p"
	, "mp S2, S3"
	]

ctxToTest :: PropContext
ctxToTest = Just ("neg", strToProp "!p -> (p -> q)", 0)

initState = defaultContext

simNeg :: IO ()
simNeg = do
	let (Right steps) = evalState (simulate stepsToTest ctxToTest) initState
	putStrLn $ concat $ map (\(PropT _ b, _) -> show b ++ "\n\n") steps

