-- Main Function

import Logix.PropEval
import System.Console.Haskeline
import Control.Monad.State

-- Start loop
main :: IO ()
main = runInputT defaultSettings $ loop defaultPropCtx defaultContext

-- Outer Loop
loop :: MonadException m => PropContext -> (SymbolContext, StepRecord) -> InputT m ()
loop propCtx ctx = do
	-- If we are proving something, we add its name before the prompt
	maybeInput <- getInputLine (case maybePropName propCtx of
		Nothing -> "> "
		Just p  -> p ++ " > ")
	-- Check the special input and null input
	case maybeInput of
		Nothing     -> return ()
		Just "quit" -> return ()
		Just ""     -> repl Nothing propCtx ctx
		Just input  -> repl (Just input) propCtx ctx

-- Inner Loop
repl :: MonadException m => Maybe String -> PropContext -> (SymbolContext, StepRecord) -> InputT m ()
repl mInput propCtx ctx = do
	-- Evaluate the input and get feedback under current context
	let (eFeedback, ctx') = runState (evaluate mInput propCtx) ctx

	-- Normal feedback or error
	case eFeedback of
		Right (mOutput, propCtx') -> do
			if mOutput /= Nothing then do -- Output only if necessary
				let (Just res) = mOutput
				if length res > 0 then outputStrLn res
					else return ()
				else return ()
			loop propCtx' ctx' -- Next loop with new context
		Left errorMsg -> do
			outputStrLn $ "[ERROR]: " ++ errorMsg
			loop propCtx ctx -- Do nothing, roll back to the old context