{-
    Unwrap the "indirect" application into primitive operations, i.e., L1, L2, L3 + MP

    The commonly used indirect applications include:

    - Dedection Theorem (with Assumptions)
    - Negative Front Theorem (p -> (!p -> q))
    - etc.
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

type Trace = (Formula, Int) -- The traced expression and its index
type ScanningState = ([Step], [Step], Trace, Formula, Int) -- Orignal Steps, New Steps, Tracer Pair, Assumption, Step Counter
type Index = Int
emptyScanningState = ([], [], (Empty, 0), Empty, 0)

-- With initial implied
unwrapDeduction :: [Step] -> Formula -> [Step]
unwrapDeduction steps assump = unwrapDeduction' $ makeInitial steps assump

-- From current state to next state
unwrapDeduction' :: ScanningState -> [Step]
unwrapDeduction' (thisStep : oSteps, nSteps, (lastTracedF, tIndex), assumption, newIndex) = 
    unwrapDeduction' (oSteps', nSteps', pair, assumption, newIndex')
    where
        (oSteps', nSteps', pair, newIndex') = case checkUsed thisStep (lastTracedF, tIndex) of
            Just (a, b, bi) -> (oSteps''
                               , nSteps ++
                                    [ Step (formulaToProp f1) s1 newIndex
                                    , Step (formulaToProp f2) s2 (newIndex + 1)
                                    , Step (formulaToProp f3) s3 (newIndex + 2)]
                               , (lastTracedF', tIndex'), newIndex + 3)
                where
                    f1 = Imply assumption (Imply a b)
                    f2 = Imply (Imply assumption a) (Imply assumption b)
                    f3 = Imply assumption b
                    s1 = Strategy HarmlessPre []
                    s2 = Strategy L2MP [newIndex]
                    s3 = Strategy MpRule [tIndex, newIndex + 1]
                    lastTracedF' = b
                    tIndex' = bi
                    oSteps'' = let (s : oSteps''') = oSteps in if unStep s == b then oSteps''' else error "not implemented yet"

            Nothing -> (oSteps, nSteps ++ [thisStep], (lastTracedF, tIndex), newIndex + 1)

unwrapDeduction' ([], nS, _, _, _) = nS

-- Change "B" into "p -> B", FIXME: This only works at "closely" next step
changeConclusion :: ([Step], (Formula, Int)) -> Formula -> Trace -> ([Step], (Formula, Int))
changeConclusion ([], x) _ _ = ([], x)
changeConclusion original@((Step (PropT _ f) strat oi : ss), x) assumption (lastTracedF, _) =
    if f == lastTracedF then
        ((Step (formulaToProp $ Imply assumption lastTracedF) strat oi) : ss, x)
        else original

-- Check if current step use the lasted traced formula as the condition of implication
checkUsed   :: Step -> Trace -> Maybe (Formula, Formula, Int)
checkUsed (Step (PropT args stepbody) (Strategy sk is) i) (lastTracedF, _) =    case stepbody of
    Imply a b -> if a == lastTracedF then Just (a, b, i) else Nothing
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
    (ss, [], (a, 0), assumption, 0)
    else makeInitial3 assumption a ss

-- Insert the scanned step
insertStep :: Step -> ScanningState -> ScanningState
insertStep s (oss, nss, ltf, ti, i) = (s:oss, nss, ltf, ti, i + 1)

---------- / UNWRAPPING BASED ON PROOF / ------------

unwrapSteps :: M.Map StrategyKind Proof -> [Step] -> [Step]
unwrapSteps proofs steps = fst $ foldr transform ([], 0) steps
    where
        transform :: Step -> ([Step], Int) -> ([Step], Int)
        transform step (ss, i) = (s' ++ ss, i + length s' - 1)
            where
                Proof _ s' = unwrapStep step

        unwrapStep :: Step -> Proof
        unwrapStep step@(Step prop@(PropT args stepbody) (Strategy sk _) si) =
            case M.lookup sk proofs of
                Just proof -> unwrapArgs proof si (map Term args)
                Nothing -> Proof prop [step]

unwrapArgs :: Proof -> Index -> [Formula] -> Proof
unwrapArgs proofDef@(Proof (PropT proofSlots proofDefBody) steps) offset args =
    case ifSameLength args proofSlots of
        Right _ -> Proof (PropT args' proofDefBody') steps'
        Left _ -> error "Wrong Unwrapping"
        where
            replacer = \toChange slots -> foldr replaceIn toChange $ zip slots args
            proofDefBody' = replacer proofDefBody proofSlots
            args' = termsToNames args
            steps' = map (\(Step (PropT slots body) stratInst i) -> Step (PropT args' $ replacer body slots) (updateStrat stratInst) (i + offset)) steps
            updateStrat :: Strategy -> Strategy
            updateStrat (Strategy sk is) = Strategy sk $ map (+ offset) is
