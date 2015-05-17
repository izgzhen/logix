module Logix.Config where
import Data.Map as M
import Logix.Definition
import Logix.Transform

skMap' = M.fromList
    [ ("mp", MpRule)
    , ("negfront", Negfront)
    , ("L1", L1)
    , ("L2", L2)
    , ("L3", L3)
    , ("hp", HarmlessPre)
    , ("L2mp", L2MP)
    , ("assume", Assume)
    ] :: M.Map String StrategyKind

skMap = M.fromList
    [ (MpRule, "mp")
    , (Negfront, "negfront")
    , (L1, "L1")
    , (L2, "L2")
    , (L3, "L3")
    , (HarmlessPre, "hp")
    , (L2MP, "L2mp")
    , (Assume, "assume")
    ] :: M.Map StrategyKind String

-- Preload axioms, constructing the basic axomatic system
preloadedAxioms = M.fromList [
      ("L1", (strToProp "p -> (q -> p)", []))
    , ("L2", (strToProp "(p -> (q -> r)) -> ((p -> q) -> (p -> r))", []))
    , ("L3", (strToProp "(p -> q) -> (!q -> !p)", []))
    ] :: SymbolContext
