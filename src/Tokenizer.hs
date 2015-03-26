module Tokenizer where
import Text.Regex.Posix
import Data.Char
import qualified Data.Map as Map
import Data.Maybe

data Keyword = Type | Ind | Def | Prop | Prove | If | Then | Check | Axiom | Theorem deriving (Show, Eq, Enum)

data Syntactic = Eq | Of | Guard | Curry | LP | RP | Comma | Neg | Ax deriving (Show, Eq, Enum)

data Token = TkKey Keyword
           | TkTac Tactic
           | TkSyn Syntactic
           | TkIdent String
           deriving (Show, Eq)

data Tactic = Intro | Apply | Reflex | Qed | Mp deriving (Show, Eq, Enum)

keywords = Map.fromList $ map (\k -> (show k, k)) [Type ..]
tactics  = Map.fromList $ map (\t -> ((\s -> (toLower $ head s) : tail s) (show t), t)) [Intro ..]

tokenize :: String -> [ Token ]
tokenize "" = []
tokenize str@(c:cs)
    | c == ':'  = TkSyn Of : tokenize cs
    | c == '('  = TkSyn LP : tokenize cs
    | c == ')'  = TkSyn RP : tokenize cs
    | c == '='  = TkSyn Eq : tokenize cs
    | c == ','  = TkSyn Comma : tokenize cs
    | c == '!'  = TkSyn Neg   : tokenize cs
    | c == '-'  = matchSlash str
    | c == '|'  = matchBar str
    | isSpace c = tokenize cs
    | isAlpha c = matchAlpha str
    | otherwise = error $ "parsing error at " ++ str


matchAlpha :: String -> [ Token ]
matchAlpha "" = []
matchAlpha str = matchHead (\c -> isAlpha c || c `elem` "\'_" || isDigit c) matchAlpha' str

matchHead :: (Char -> Bool) -> (String -> Token) -> String -> [ Token ]
matchHead f g str
    | word == "" = [g rest]
    | otherwise  = g word : tokenize rest
  where
    (word, rest) = span f str

matchAlpha' :: String -> Token
matchAlpha' word
    | maybeKey /= Nothing = TkKey $ fromJust maybeKey
    | maybeTac /= Nothing = TkTac $ fromJust maybeTac
    | otherwise         = TkIdent word
    where
        maybeKey = Map.lookup word keywords
        maybeTac = Map.lookup word tactics

matchSlash :: String -> [ Token ]
matchSlash = matchHead (\c -> c `elem` "->") matchSlash'

matchSlash' :: String -> Token
matchSlash' operator
    | operator == "->" = TkSyn Curry
    | otherwise        = error $ "No such slash word" ++ operator

matchBar :: String -> [ Token ]
matchBar "" = []
matchBar str
    | length str < 3 = error $ "No such bar " ++ str
    | str !! 1 == '-'  = TkSyn Ax : tokenize (drop 2 str)
    | otherwise = TkSyn Guard : tokenize (drop 1 str)