-- Propsitioinal Logic Assistant
-- Building Axioms, Applying MP rules and theorems/axioms
-- REPL Looping

import PropEval
import System.Console.Haskeline
import Control.Monad.State

defaultPrompt = "> "

main :: IO ()
main = runInputT defaultSettings $ loop defaultContext defaultPrompt Nothing
    where
        loop ctx prompt maybeProp = do
            minput <- getInputLine prompt
            case minput of
                Nothing     -> return ()
                Just "quit" -> return ()
                Just input  -> repl input ctx prompt maybeProp

        repl input ctx prompt maybeProp = do
            if input == "exit" then do
                outputStrLn "Exiting.."
                return ()
            else do
                let ((maybeRes, maybePrompt, maybeProp'), ctx') = runState (evaluate input maybeProp) ctx
                case maybeRes of
                    Nothing  -> return ()
                    Just res -> outputStrLn res
                let prompt' = case maybePrompt of
                        Nothing -> prompt
                        Just p  -> p ++ " > "
                loop ctx' prompt' maybeProp'