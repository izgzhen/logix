module Logix.Definition where
import qualified Data.Map as M
import Control.Monad.State
import Logix.Utils

data Formula = Imply Formula Formula
             | Term String
             | Not Formula
             | Empty deriving (Eq)

type FormalArguments = [String]
data PropT = PropT FormalArguments Formula deriving (Show, Eq) -- arguments and body

type Index = Int
data StrategyKind = MpRule | Negfront | L1 | L2 | L3 | HarmlessPre | L2MP | Assume | IdRule deriving (Show, Ord, Eq)
data Strategy = Strategy StrategyKind [Index] -- Name and its arguments
data Step = Step PropT Strategy Index

type SymbolContext = M.Map String (PropT, [Step]) -- Map name to proposition
type StepRecord = M.Map Int Step -- Record steps during proving
type Evaluator a = State (SymbolContext, StepRecord) a -- Context Wrapper
type PropContext = Maybe (String, PropT, Int) -- Proposition information which is in being proved

data Proof = Proof PropT [Step] deriving (Show) -- Definition and proving steps

----- / PRETTIFY / ----

instance Show Formula where
    show (Term str) = str
    show (Not f) = "¬" ++ show f
    show (Imply front end) = "(" ++ show front ++ " → " ++ show end ++ ")"
    show Empty = ""

instance Show Strategy where
    show (Strategy sk []) = show sk
    show (Strategy sk skargs) = show sk ++ " on " ++ prettifySkArgs skargs

instance Show Step where
    show (Step (PropT _ propBody) sk stepi) = show stepi ++ " : " ++ show propBody ++ " with " ++ show sk
