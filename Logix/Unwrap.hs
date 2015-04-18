{-
	Unwrap the "indirect" application into primitive operations, i.e., L1, L2, L3 + MP

	The commonly used indirect applications include:

	- Dedection Theorem (with Assumptions)
	- Negative Front Theorem (p -> (!p -> q))
	- etc.
-}

{-
	TODOs:

	1. Make some functions which could facilitate the testings
	2. Test the correctness of the mp rule unwrapping prototype
	3. Add necessary indexing information to the types
	4. Add the indexing features into the mp unwrapping
	5. Test the correctness of mp unwrapping with indexing
-}

module Logix.Unwrap where
import Logix.PropParser
import Logix.Tokenizer
import Logix.PropContext
import Logix.PropTransform
import Logix.Utils
import qualified Data.Map as M

data Proof = Proof PropT [Step] deriving (Show)

type ScanningState = ([Step], [Step], (Formula, Int), Formula) -- Orignal Steps, New Steps, Tracer Pair, Assumption
emptyScanningState = ([], [], (Empty, 0), Empty)

transformMp :: Step -> Formula -> Int -> Formula -> [Step] -> ([Step], (Formula, Int))
checkUsed   :: Step -> Formula -> Bool

checkUsed (PropT args stepbody, Strategy sk is) lastTracedF = case stepbody of
	Imply a b -> if a == lastTracedF then True else False
	_ -> False

transformMp (PropT args stepbody@(Imply a b), Strategy sk is) lastTracedF tIndex assumption nSteps =
	(nSteps', (lastTracedF', tIndex'))
	where
		f1 = Imply assumption stepbody
		f2 = Imply (Imply assumption a) (Imply assumption b)
		s1 = Strategy HarmlessPre []
		s2 = Strategy L2MP []
		nSteps' = nSteps ++ [(formulaToProp $ f1, s1), (formulaToProp $ f2, s2)]
		lastTracedF' = b
		tIndex' = 0 -- Just Temporily, but it seems that it is necessary to attach step with index, ouch!

unwrapMp :: [Step] -> Formula -> [Step]
unwrapMp steps assump = unwrapMp' $ makeInitial steps assump


makeInitial :: [Step] -> Formula -> ScanningState
makeInitial [] _ = emptyScanningState
makeInitial (s:ss) assump = if unStep s == assump then makeInitial2 assump ss
	else insertStep s $ makeInitial ss assump


insertStep :: Step -> ScanningState -> ScanningState
insertStep s (oss, nss, ltf, ti) = (s:oss, nss, ltf, ti)

makeInitial2 assumption [] = emptyScanningState
makeInitial2 assumption (s:ss) = case unStep s of
	Imply assumption a -> (s:ss, [], (a, 0), assumption)
	_ -> insertStep s $ makeInitial2 assumption ss


unwrapMp' :: ScanningState -> [Step]
unwrapMp' ([], nS, _, _) = nS
unwrapMp' (oSteps, nSteps, (lastTracedF, tIndex), assumption) = unwrapMp' (oSteps', nSteps', (lastTracedF', tIndex'), assumption)
	where
		thisStep : oSteps' = oSteps
		(nSteps', (lastTracedF', tIndex')) = if checkUsed thisStep lastTracedF then
			transformMp thisStep lastTracedF tIndex assumption nSteps
		else
			(nSteps ++ [thisStep], (lastTracedF, tIndex))

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
stepsToTest = [ (formulaToProp $ Term "p", Strategy Assume [0])
	, (formulaToProp $ Imply (Term "p") (Term "a"), Strategy MpRule [0])
	, (formulaToProp $ Imply (Term "p") (Imply (Term "p") (Term "a")), Strategy Negfront [2]) ]