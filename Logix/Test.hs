module Logix.Test where
import Logix.PropTransform
import Logix.PropDefs

proofToTest :: [String]
proofToTest = [
      "L1 p, (p -> p)"
    , "L2 p, (p -> p), p"
    , "mp S0, S1"
    , "L1 p, p"
    , "mp S2, S3"
    ]

propToTest :: PropContext
propToTest = Just ("id_rule", strToProp "p -> p", 0)