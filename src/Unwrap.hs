{-
	Unwrap the "indirect" application into primitive operations, i.e., L1, L2, L3 + MP

	The commonly used indirect applications include:

	- Dedection Theorem (with Assumptions)
	- Negative Front Theorem (p -> (!p -> q))
	- etc.
-}

module Unwrap where
import PropParser
import Tokenizer

type step = (Formula, String)

mpUnwrap :: [Formula] -> [step] -> [step]
mpUnwrap assumptions steps steps' = undefined