module InferTest where

import Data.Set (fromList)
import Inferencer
import Parsetree
import Scheme hiding (free_vars, occurs_in)
import qualified System.Exit as Exit
import Test.HUnit
import Typedtree

infer1id :: Test
infer1id =
  TestCase $
    assertEqual
      ""
      -- (Scheme.Scheme (Data.Set.fromList [1]) (Arrow (TyVar 1) (TyVar 1)))
      (Arrow (TyVar 0) (TyVar 0))
      (runInfer (ELam (PVar "x") (EVar "x")))

inferTests :: Test
inferTests = TestList [infer1id]

tests :: Test
tests = TestList [TestLabel "InferTests" inferTests]
