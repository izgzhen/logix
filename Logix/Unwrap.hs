{-
    Unwrap the "indirect" application into primitive operations, i.e., L1, L2, L3 + MP

    The commonly used indirect applications include:

    - Dedection Theorem (with Assumptions)
    - Negative Front Theorem (p -> (!p -> q))
    - etc.
-}

{-
    TODOs:

    1. Make some functions which could facilitate the testings -- Done
    2. Test the correctness of the mp rule unwrapping prototype -- Done
    3. Add necessary indexing information to the types
    4. Add the indexing features into the mp unwrapping
    5. Test the correctness of mp unwrapping with indexing
-}

module Logix.Unwrap where
import Logix.PropDefs
import Logix.PropParser
import Logix.Tokenizer
import Logix.PropContext
import Logix.PropTransform
import Logix.Utils
import qualified Data.Map as M

--------------- / UNWRAPPING BASED ON DEDUCTION / ---------------

type ScanningState = ([Step], [Step], (Formula, Int), Formula) -- Orignal Steps, New Steps, Tracer Pair, Assumption
emptyScanningState = ([], [], (Empty, 0), Empty)

-- With initial implied
unwrapDeduction :: [Step] -> Formula -> [Step]
unwrapDeduction steps assump = unwrapDeduction' $ makeInitial steps assump

-- From current state to next state
unwrapDeduction' :: ScanningState -> [Step]
unwrapDeduction' (thisStep : oSteps, nSteps, (lastTracedF, tIndex), assumption) = 
    unwrapDeduction' (oSteps', nSteps', pair, assumption)
    where
        (oSteps', nSteps', pair) = case checkUsed thisStep lastTracedF of
            Just (a, b) -> (oSteps'', nSteps ++ [(formulaToProp $ f1, s1), (formulaToProp $ f2, s2), (formulaToProp $ f3, s3)], (b, 0))
                where
                    f1 = Imply assumption (Imply a b)
                    f2 = Imply (Imply assumption a) (Imply assumption b)
                    f3 = Imply assumption b
                    s1 = Strategy HarmlessPre []
                    s2 = Strategy L2MP []
                    s3 = Strategy MpRule []

                    oSteps'' = let (s : oSteps''') = oSteps in if unStep s == b then oSteps''' else error "not implemented yet"

            Nothing -> (oSteps, nSteps ++ [thisStep], (lastTracedF, tIndex))

unwrapDeduction' ([], nS, _, _) = nS

-- Change "B" into "p -> B", FIXME: This only works at "closely" next step
changeConclusion :: ([Step], (Formula, Int)) -> Formula -> Formula -> ([Step], (Formula, Int))
changeConclusion ([], x) _ _ = ([], x)
changeConclusion original@(((PropT _ f, strat) : ss), x) assumption lastTracedF =
    if f == lastTracedF then
        (((formulaToProp $ Imply assumption lastTracedF, strat) : ss), x)
        else original

-- Check if current step use the lasted traced formula as the condition of implication
checkUsed   :: Step -> Formula -> Maybe (Formula, Formula)
checkUsed (PropT args stepbody, Strategy sk is) lastTracedF = case stepbody of
    Imply a b -> if a == lastTracedF then Just (a, b) else Nothing
    _ -> Nothing

-- Capture the assumption and enter intialization stage 2
makeInitial :: [Step] -> Formula -> ScanningState
makeInitial [] _ = emptyScanningState
makeInitial (s:ss) assump = if unStep s == assump then makeInitial2 assump ss
    else insertStep s $ makeInitial ss assump

-- Capture the first implication involving assumption
makeInitial2 assumption [] = emptyScanningState
makeInitial2 assumption (s:ss) = case unStep s of
    Imply assumption a -> insertStep s $ makeInitial3 assumption a ss
    _ -> insertStep s $ makeInitial2 assumption ss

-- Drop the orginal conclusion
makeInitial3 assumption _ [] = emptyScanningState
makeInitial3 assumption a (s:ss) = if unStep s == a then
    (ss, [], (a, 0), assumption)
    else makeInitial3 assumption a ss

-- Insert the scanned step
insertStep :: Step -> ScanningState -> ScanningState
insertStep s (oss, nss, ltf, ti) = (s:oss, nss, ltf, ti)


---------- / UNWRAPPING BASED ON PROOF / ------------

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
        updateStrat (Strategy sk is) = Strategy sk $ map (+ offset) is
