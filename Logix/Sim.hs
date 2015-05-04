module Logix.Sim where
import Logix.PropDefs
import Logix.PropContext
import Logix.PropEval
import Logix.Utils
import Logix.PropTransform
import Logix.Tokenizer
import Control.Monad.State

import qualified Data.Map as M

skMap = M.fromList
    [ (MpRule, "mp")
    , (Negfront, "negfront")
    , (L1, "L1")
    , (L2, "L2")
    , (L3, "L3")
    , (HarmlessPre, "hp")
    , (L2MP, "L2mp")
    , (Assume, "assume")
    ] :: M.Map StrategyKind String

unTokenize :: [(String, StrategyKind)] -> [String]
unTokenize [] = []
unTokenize ((s, sk):ps) = p' ++ unTokenize ps
    where
        p' = case M.lookup sk skMap of
            Just str -> [str ++ " " ++ s]
            Nothing  -> [s]

simulate' :: [(String, StrategyKind)] -> PropContext -> Evaluator (EitherS [Step])
simulate' sp = simulate $ unTokenize sp

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
            putStrLn $ concat $ map (\(PropT _ b, _) -> show b ++ "\n") steps
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
        _ -> putStrLn "Mistakes in proving"

