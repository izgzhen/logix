module Tokenizer where
import Text.Regex.Posix
import Data.Char
import qualified Data.Map as Map
import Data.Maybe

data Keyword = Type | Ind | Def | Prop | Prove | If | Then deriving (Show, Eq)
keywords = Map.fromList [("Type", Type), ("Ind", Ind), ("Def", Def), ("Prop", Prop), ("Prove", Prove), ("if", If), ("then", Then)]

data Syntactic = Eq | Of | Guard | Curry | LP | RP | Comma deriving (Show, Eq)

data Token = TkKey Keyword
           | TkTac Tactic
           | TkSyn Syntactic
           | TkIdent String
           deriving (Show, Eq)

data Tactic = Intro | Apply | Reflex | Qed deriving (Show, Eq)
tactics = Map.fromList [("intro", Intro), ("apply", Apply), ("reflex", Reflex), ("qed", Qed)]

type Block = [(Int, String)]    -- Indentation Level and the line's content
type TBlock = [(Int, [Token])]

stripper :: String -> (Int, String)
stripper line = (tabs, stripped)
    where
        stripped = dropWhile (=='\t') line
        tabs = length line - length stripped

blocker :: Block -> [ Block ] -- From a whole "block"
blocker [] = []
blocker lns = b : blocker rest
    where
        headLn = head lns
        (b', rest) = span (\ln -> fst ln /= 0) $ tail lns
        b = headLn:b'

process :: String -> [ Block ]
process txt = blocker . map stripper $ filter (/= "") . lines $ txt

constructBlock :: Block -> TBlock
constructBlock block = map (\(i, str) -> (i, tokenize str)) block

readTokens filename = do
    text <- readFile filename
    return $ map constructBlock $ process text

tokenize :: String -> [ Token ]
tokenize "" = []
tokenize str@(c:cs)
    | c == '|'  = TkSyn Guard : tokenize cs
    | c == ':'  = TkSyn Of : tokenize cs
    | c == '('  = TkSyn LP : tokenize cs
    | c == ')'  = TkSyn RP : tokenize cs
    | c == '='  = TkSyn Eq : tokenize cs
    | c == ','  = TkSyn Comma : tokenize cs
    | c == '-'  = matchSlash str
    | isSpace c = tokenize cs
    | isAlpha c = matchAlpha(str)
    | otherwise = error $ "parsing error at " ++ str

matchAlpha :: String -> [ Token ]
matchAlpha "" = []
matchAlpha str = matchHead (\c -> isAlpha c || c `elem` "\'_") matchAlpha' str

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