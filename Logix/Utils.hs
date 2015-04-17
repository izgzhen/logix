-- Helpers for Error Handling, just the type signatures will be meaningful

module Logix.Utils (
	(<|>),
	(<||>),
	(<|||>),
	(<||||>),
	(<|||||>),
	unique
	) where
import Control.Monad
import Data.List

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

unique :: (Eq a, Ord a) => [a] -> [a]
unique = map (\(x:_) -> x) . group . sort