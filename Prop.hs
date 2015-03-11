-- Propsitioinal Logic Assistant
-- Building Axioms, Applying MP rules and theorems/axioms
-- REPL Looping

import PropEval
import System.Console.Haskeline
import Control.Monad.State

main :: IO ()
main = runInputT defaultSettings $ loop defaultContext
	where
		loop ctx = do
			minput <- getInputLine "> "
			case minput of
				Nothing     -> return ()
				Just "quit" -> return ()
				Just input  -> repl input ctx

		repl input ctx = do
			if input == "exit" then do
				outputStrLn "Exiting.."
				return ()
			else do
				let (s, ctx') = runState (evaluate input) ctx
				outputStrLn s
				loop ctx'