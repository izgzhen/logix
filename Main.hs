-- Main Function

import Logix.PropEval
import System.Console.Haskeline
import Control.Monad.State

-- Start loop
main :: IO ()
main = runInputT defaultSettings $ loop defaultPropCtx defaultContext

-- Outer Loop
loop :: MonadException m => PropContext -> SymbolContext -> InputT m ()
loop propCtx sCtx = do
	-- If we are proving something, we add its name before the prompt
	maybeInput <- getInputLine (case maybePropName propCtx of
		Nothing -> "> "
		Just p  -> p ++ " > ")
	-- Check the special input and null input
	case maybeInput of
		Nothing     -> return ()
		Just "quit" -> return ()
		Just ""     -> repl Nothing propCtx sCtx
		Just input  -> repl (Just input) propCtx sCtx

-- Inner Loop
repl :: MonadException m => Maybe String -> PropContext -> SymbolContext -> InputT m ()
repl mInput propCtx sCtx = do
	-- Evaluate the input and get feedback under current context
	let (eFeedback, sCtx') = runState (evaluate mInput propCtx) sCtx

	-- Normal feedback or error
	case eFeedback of
		Right (mOutput, propCtx') -> do
			if mOutput /= Nothing then do -- Output only if necessary
				let (Just res) = mOutput
				if length res > 0 then outputStrLn res
					else return ()
				else return ()
			loop propCtx' sCtx' -- Next loop with new context
		Left errorMsg -> do
			outputStrLn $ "[ERROR]: " ++ errorMsg
			loop propCtx sCtx -- Do nothing, roll back to the old context