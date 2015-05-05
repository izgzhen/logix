module Logix.Test where
import Logix.PropTransform
import Logix.PropDefs
import Logix.Sim
import Logix.Unwrap
import qualified Data.Map as M

--------- / AUTOMATIC PROVING / -----------

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

testId :: IO ()
testId = prove idRuleProof idRuleGoal

--------- / UNWRAP PROCESS PROVING / -------

{-
  Currently, the chronological order is strictly limited,
-}

unwrapStepsExample :: [Step]
unwrapStepsExample = [
    (strToProp "p", Strategy Assume [])
  , (strToProp "p -> A", Strategy L1 [])
  , (strToProp "A", Strategy MpRule [0, 1])
  , (strToProp "A -> B", Strategy L1 [])
  , (strToProp "B", Strategy MpRule [2, 3])
    ]

unwrapAssumpExample :: Formula
unwrapAssumpExample = Term "p"

unwrapTest = unwrapDeduction unwrapStepsExample unwrapAssumpExample

----------- / UNWRAP BASED ON PROOF EXAMPLE / -------------


unwrapProofBasedProofs = M.fromList [
    (Id_rule, Proof (strToProp "p -> p") [
      (strToProp "(p -> ((p -> p) -> p))", Strategy L1 [])
    , (strToProp "((p -> ((p -> p) -> p)) -> ((p -> (p -> p)) -> (p -> p)))", Strategy L2 [])
    , (strToProp "((p -> (p -> p)) -> (p -> p))", Strategy MpRule [0, 1])
    , (strToProp "(p -> (p -> p))", Strategy L1 [])
    , (strToProp "p -> p", Strategy MpRule [2, 3])
    ])
  ]

unwrapProofBasedSteps = [(strToProp "p -> p", Strategy Id_rule [])]
