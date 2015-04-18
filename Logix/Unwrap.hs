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
import qualified Data.Map as M

data Proof = Proof PropT [Step] deriving (Show)

unwrapSteps :: M.Map StrategyKind Proof -> [Step] -> [Step]
unwrapSteps proofs steps = fst $ foldr transform ([], 0) steps
	where
		transform :: Step -> ([Step], Int) -> ([Step], Int)
		transform step (ss, i) = (s' ++ ss, i')
			where
				i' = i + length s' - 1
				Proof _ s' = unwrapStep step i'
		unwrapStep :: Step -> Int -> Proof
		unwrapStep step@(prop@(PropT args stepbody), Strategy sk _) index = case M.lookup sk proofs of
			Just proof -> unwrapArgs proof (map Term args) index
			Nothing -> Proof prop [step]

unwrapArgs :: Proof -> [Formula] -> Int -> Proof
unwrapArgs proofDef@(Proof (PropT proofSlots proofDefBody) steps) args offset = case ifSameLength args proofSlots of
	Right _ -> Proof (PropT args' proofDefBody') steps'
	Left _ -> error "Wrong Unwrapping"
	where
		replacer = \toChange slots -> foldr replaceIn toChange $ zip slots args
		proofDefBody' = replacer proofDefBody proofSlots
		args' = termsToNames args
		steps' = map (\(PropT slots body, stratInst) -> (PropT args' $ replacer body slots, updateStrat stratInst)) steps
		updateStrat :: Strategy -> Strategy
		updateStrat (Strategy sk is) = Strategy sk $ map (+offset) is

proofsToApply :: M.Map StrategyKind Proof
proofsToApply = M.fromList [
		(Negfront, Proof (PropT ["p", "q"] (Imply (Term "p") (Term "q")))	
			[ (formulaToProp $ Imply (Term "p") (Term "p"), Strategy L1 [0])
			, (formulaToProp $ Imply (Term "p") (Term "q"), Strategy L2 [1])
			, (formulaToProp $ Imply (Term "p") (Imply (Term "p") (Term "q")), Strategy L3 [2])
			])
	]

argsToTest = [Term "a", Term "b"]

stepsToTest :: [Step]
stepsToTest = [ (formulaToProp $ Imply (Term "a") (Term "a"), Strategy L1 [0])
	, (formulaToProp $ Imply (Term "a") (Term "b"), Strategy L2 [1])
	, (formulaToProp $ Imply (Term "a") (Imply (Term "a") (Term "b")), Strategy Negfront [2]) ]