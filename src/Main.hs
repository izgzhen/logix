import PropEval
import System.Console.Haskeline
import Control.Monad.State

-- 
main :: IO ()
main = runInputT defaultSettings $ loop defaultPropCtx defaultContext

loop :: MonadException m => PropContext -> SymbolContext -> InputT m ()
loop propCtx sCtx = do
	maybeInput <- getInputLine (case maybePropName propCtx of
		Nothing -> "> "
		Just p  -> p ++ " > ")
	case maybeInput of
		Nothing     -> return ()
		Just "quit" -> return ()
		Just ""     -> repl Nothing propCtx sCtx
		Just input  -> repl (Just input) propCtx sCtx

repl :: MonadException m => Maybe String -> PropContext -> SymbolContext -> InputT m ()
repl mInput propCtx sCtx = do
	let (eFeedback, sCtx') = runState (evaluate mInput propCtx) sCtx
	case eFeedback of
		Right (mOutput, propCtx') -> do
			if mOutput /= Nothing then do
				let (Just res) = mOutput
				if length res > 0 then outputStrLn res
					else return ()
				else return ()
			loop propCtx' sCtx'
		Left errorMsg -> do
			outputStrLn $ "[ERROR]: " ++ errorMsg
			loop propCtx sCtx