module Logix.Utils where
import Control.Monad
import Data.List


-- Helpers for Error Handling, just the type signatures will be meaningful
-- FIXME: You should use some monadic functions to do that, or maek a new context monad,
--        rather than using a lot of meaningless monadic operators
type EitherS = Either String

(<|>) :: (Monad m) => EitherS b -> (b -> c) -> m (EitherS c)
(<||>) :: (Monad m) => EitherS b -> (b -> m c) -> m (EitherS c)
(<|||>) :: (Monad m) => m (EitherS b) -> (b -> c) -> m (EitherS c)
(<||||>) :: (Monad m) => EitherS b -> (b -> m (EitherS c)) -> m (EitherS c)
(<|||||>) :: (Monad m) => m (EitherS b) -> (b -> m (EitherS c)) -> m (EitherS c)

(<|>) val g = return $ case val of
    Right bVal -> Right (g bVal)
    Left err -> Left err

(<||>) val g = case val of
        Right bVal -> liftM Right $ g bVal
        Left err -> return $ Left err

(<|||>) val g = val >>= (flip (<|>) g)

(<||||>) val g = case val of
        Right bVal -> (g bVal) <|||> id
        Left err -> return $ Left err

(<|||||>) val g = val >>= (\val' -> case val' of
        Right bVal -> (g bVal) <|||> id
        Left err -> return $ Left err)

-- Drop the duplicated elements in a list
unique :: (Eq a, Ord a) => [a] -> [a]
unique = map (\(x:_) -> x) . group . sort

ifSameLength :: [a] -> [b] -> EitherS ()
ifSameLength as bs = if length as == length bs
    then return ()
    else fail "not of same length"

prettifySkArgs [] = ""
prettifySkArgs (a:[]) = sShow a
prettifySkArgs (a:as) = sShow a ++ ", " ++ prettifySkArgs as
sShow x = "S" ++ show x