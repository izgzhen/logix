{-
	Unwrap the "indirect" application into primitive operations, i.e., L1, L2, L3 + MP

	The commonly used indirect applications include:

	- Dedection Theorem (with Assumptions)
	- Negative Front Theorem (p -> (!p -> q))
	- etc.
-}

module Logix.Unwrap where
import Logix.PropParser
import Logix.Tokenizer
import Logix.PropContext
import Logix.PropTransform
import Logix.Utils

type Proof = (PropT, [Step])

unwrap :: Proof -> [Formula] -> EitherS Proof
unwrap (prop@(PropT slots propBody), steps) args = case ifSameLength args slots of
	Right () -> return (prop, map replaceStep steps)
	Left errorMsg -> fail errorMsg
	where
		replaceStep :: Step -> Step
		replaceStep (PropT slots body, stratInst) = (PropT slots body', stratInst)
			where
				prop = PropT slots body'
				body' = fillInStep slots body

		fillInStep :: FormalArguments -> Formula -> Formula
		fillInStep stepSlots stepBody = foldr replaceIn stepBody $ zip stepSlots args