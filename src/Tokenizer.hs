module Tokenizer where
import Text.Regex.Posix
import Data.Char
import qualified Data.Map as Map
import Data.Maybe
import Data.Either

type EitherS = Either String

data Keyword = Check | Axiom | Theorem deriving (Show, Eq, Enum)

data Tactic = Qed | Mp deriving (Show, Eq, Enum)

data Syntactic = Curry | LP | RP | Comma | Neg | Ax deriving (Show, Eq, Enum)

data Token = TkKey Keyword
           | TkTac Tactic
           | TkSyn Syntactic
           | TkIdent String
           deriving (Show, Eq)

transformToMap l f = Map.fromList $ map f l

keywords = transformToMap [Check ..] (\k -> (show k, k))
tactics  = transformToMap  [Qed ..] (\t -> ((\s -> (toLower $ head s) : tail s) (show t), t))
symbols  = Map.fromList [('(', TkSyn LP), (')', TkSyn RP), (',', TkSyn Comma), ('!', TkSyn Neg)]

type MapTk = Map.Map String Token

tokenize :: String -> EitherS [Token]
tokenize "" = Right []
tokenize str@(c:cs)
    | Map.member c symbols = do
        let (Just tk) = Map.lookup c symbols
        tks <- tokenize cs
        return (tk : tks)
    | isSpace c = tokenize cs
    | isAlpha c = matchAlpha str
    | c == '-' = matchSlash str
    | c == '|' = matchBar str
    | otherwise = Left $ "[tokenize] error tokenizing " ++ show str


matchHead :: (Char -> Bool) -> (String -> EitherS Token) -> String -> EitherS [Token]
matchHead f g str
  | word == "" = do
        tk <- g rest
        return [tk]
  | otherwise  = do
      restTks <- tokenize rest
      tk <- g word
      return (tk : restTks)
  where
    (word, rest) = span f str

matchAlpha :: String -> EitherS [Token]
matchAlpha "" = return []
matchAlpha str = matchHead (\c -> isAlpha c || c `elem` "\'_" || isDigit c) matchAlpha' str

matchSlash :: String -> EitherS [Token]
matchSlash = matchHead (\c -> c `elem` "->") matchSlash'

matchBar :: String -> EitherS [Token]
matchBar "" = return []
matchBar str
    | length str < 3   = fail $ "No such bar " ++ str
    | str !! 1 == '-'  = do
        tks <- tokenize (drop 2 str)
        return $ TkSyn Ax : tks

matchAlpha' :: String -> EitherS Token
matchAlpha' word
    | isMember keywords = return (TkKey $ getFrom keywords)
    | isMember tactics  = return (TkTac $ getFrom tactics)
    | otherwise         = return (TkIdent word)
    where
      isMember = Map.member word
      getFrom kv = fromJust $ Map.lookup word kv

matchSlash' :: String -> EitherS Token
matchSlash' operator
    | operator == "->" = return (TkSyn Curry)
    | otherwise        = fail $ "No such slash word" ++ operator