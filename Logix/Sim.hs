module Logix.Sim where
import Logix.PropDefs
import Logix.PropContext
import Logix.PropEval
import Logix.Utils
import Logix.Tokenizer
import qualified Data.Map as M

-- Invent a wheel just lile EitherT or InputT

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
simulate [] ctx = do
    records <- gatherRecords
    return $ Right records

simulate (s:ss) ctx = evaluate (Just s) ctx <|||||> \(_, propCtx) -> simulate ss propCtx