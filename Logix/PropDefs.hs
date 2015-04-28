module Logix.PropDefs where
import qualified Data.Map as M
import Control.Monad.State

data Formula = Imply Formula Formula
			 | Term String
			 | Not Formula
			 | Empty deriving (Eq)

instance Show Formula where
	show (Term str) = str
	show (Not f) = "¬" ++ show f
	show (Imply front end) = "(" ++ show front ++ " → " ++ show end ++ ")"
	show Empty = ""

type FormalArguments = [String]
data PropT = PropT FormalArguments Formula deriving (Show, Eq) -- arguments and body

data StrategyKind = MpRule | Negfront | L1 | L2 | L3 | HarmlessPre | L2MP | Assume deriving (Show, Ord, Eq)
data Strategy = Strategy StrategyKind [Int] deriving (Show) -- Name and its arguments

type Step = (PropT, Strategy)
type SymbolContext = M.Map String (PropT, [Step]) -- Map name to proposition
type StepRecord = M.Map Int Step -- Record steps during proving
type Evaluator a = State (SymbolContext, StepRecord) a -- Context Wrapper
type PropContext = Maybe (String, PropT, Int) -- Proposition information which is in being proved
data Proof = Proof PropT [Step] deriving (Show)