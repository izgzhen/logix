import Logix.Transform
import Logix.Definition
import Logix.Simulation
import Logix.Unwrap
import qualified Data.Map as M

--------- / TEST AUTOMATIC PROVING / -----------

idRuleProof :: [String]
idRuleProof = [
      "L1 p, (p -> p)"
    , "L2 p, (p -> p), p"
    , "mp S0, S1"
    , "L1 p, p"
    , "mp S2, S3"
    ]

idRuleGoal :: PropContext
idRuleGoal = Just ("id_rule", strToProp "p -> p", 0)

testAutomaticProving :: IO ()
testAutomaticProving = prove idRuleProof idRuleGoal

--------- / TEST UNWRAP DEDUCTION / -------

-- FIXME: Currently, the chronological order is strictly limited

constructSnippet :: [(PropT, Strategy)] -> [Step]
constructSnippet = map (\(i, (a, b)) -> (Step a b i)) . zip [0..]

unwrapStepsExample :: [Step]
unwrapStepsExample =  constructSnippet [
    (strToProp "p", Strategy Assume [])
  , (strToProp "p -> A", Strategy L1 [])
  , (strToProp "A", Strategy MpRule [0, 1])
  , (strToProp "A -> B", Strategy L1 [])
  , (strToProp "B", Strategy MpRule [2, 3])
    ]

unwrapAssumpExample :: Formula
unwrapAssumpExample = Term "p"

testUnwrapDeduction :: IO ()
testUnwrapDeduction = mapM_ print $ unwrapDeduction unwrapStepsExample unwrapAssumpExample

----------- / TEST UNWRAP BASED ON PROOF / -------------

unwrapProofBasedProofs = M.fromList [
    (IdRule, Proof (strToProp "p -> p") $ constructSnippet [
      (strToProp "(p -> ((p -> p) -> p))", Strategy L1 [])
    , (strToProp "((p -> ((p -> p) -> p)) -> ((p -> (p -> p)) -> (p -> p)))", Strategy L2 [])
    , (strToProp "((p -> (p -> p)) -> (p -> p))", Strategy MpRule [0, 1])
    , (strToProp "(p -> (p -> p))", Strategy L1 [])
    , (strToProp "p -> p", Strategy MpRule [2, 3])
    ])
  ]

unwrapProofBasedSteps = [Step (strToProp "p -> p") (Strategy IdRule []) 0]

testUnwrapProofBased :: IO ()
testUnwrapProofBased = mapM_ print $ unwrapSteps unwrapProofBasedProofs unwrapProofBasedSteps 


main :: IO ()
main = do
  print "Test Unwrapping with Proof Subsitution:"
  testUnwrapProofBased
  print "Test Unwrapping with Deduction"
  testUnwrapDeduction
  print "Test Automatic Proving:"
  testAutomaticProving