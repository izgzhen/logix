module Logix.Simulation where

import Logix.Definition
import Logix.Config
import Logix.Context
import Logix.Eval
import Logix.Utils
import Logix.Transform
import Logix.Tokenizer

import Control.Monad.State
import qualified Data.Map as M

unTokenize :: [(String, StrategyKind)] -> [String]
unTokenize [] = []
unTokenize ((s, sk):ps) = p' ++ unTokenize ps
    where
        p' = case M.lookup sk skMap of
            Just str -> [str ++ " " ++ s]
            Nothing  -> [s]

simulate' :: [(String, StrategyKind)] -> PropContext -> Evaluator (EitherS [Step])
simulate' sp = simulate $ unTokenize sp

-- Core of simulation, feeding a set of string into evaluator and simulate the usual
-- REPL loop, finally get the proved conclusion out
simulate :: [String] -> PropContext -> Evaluator (EitherS [Step])
simulate (s:ss) ctx = evaluate (Just s) ctx <|||||> \(_, propCtx) -> simulate ss propCtx
simulate [] ctx = do -- Final Step
    records <- gatherRecords
    return $ Right records


prove :: [String] -> PropContext -> IO ()
prove proof goal = do
    let result = evalState (simulate proof goal) defaultContext
    case result of
        Right steps -> do
            putStrLn "Steps:"
            putStrLn $ concat $ map (\step -> show step ++ "\n") steps
            case reverse steps of
                (s:_) -> do
                    case goal of
                        Just (goalName, goalProp, _) -> do
                            if goalProp == formulaToProp (unStep s) then
                                putStrLn $ "Proved: [" ++ goalName ++ "]"
                            else
                                putStrLn "Not proved"
                        _ -> putStrLn "Invalid Goal"
                _ -> putStrLn "No steps!"
        Left errMsg -> putStrLn $ "Mistakes in proving: " ++ errMsg
