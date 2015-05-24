module Logix.Transform where
import Logix.Definition
import Logix.Parser
import Logix.Tokenizer
import Logix.Utils
import Data.Char
import Data.List
import Data.Either
import qualified Data.Map as M
import Control.Monad
import Control.Monad.State

-------- PURE FUNCTIONS ----------------

-- Replace matched slot with content in a certain body
replaceIn :: (String, Formula) -> Formula -> Formula
replaceIn (slot, content) body = checkFormula body
    where
        checkFormula (Imply f1 f2) = ((r' f1) --> (r' f2))
        checkFormula formula@(Term str) = if slot == str then content else formula
        checkFormula Empty = Empty
        checkFormula (Not f) = Not $ r' f
        r' = replaceIn (slot, content)

-- Extract all appeared arguments from a formula body
extractArgs :: Formula -> [String]
extractArgs Empty = []
extractArgs (Imply f1 f2) = unique $ extractArgs f1 ++ extractArgs f2
extractArgs (Term str) = [str]
extractArgs (Not f) = unique $ extractArgs f

-- Transform a formula to a proposition
formulaToProp :: Formula -> PropT
formulaToProp f = PropT (extractArgs f) f

-- Get the core proposition from a intricate step
unStep :: Step -> Formula
unStep (Step (PropT _ body) _ _) = body

-- The fellowing two are purely for testing usage

strToProp :: String -> PropT
strToProp str = formulaToProp . strToFormula $ str

strToFormula :: String -> Formula
strToFormula str = case tokenize str >>= parseFormula of
        Right (formula, []) -> formula
        _                   -> error $ "illegal string to formula: " ++ str
