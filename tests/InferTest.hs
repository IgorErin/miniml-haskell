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
  TestCase $ do
    case runInfer term of
      Left e -> assertFailure ("No type: " ++ show e)
      Right tyAns -> assertEqual "" ty tyAns
  where
    ty = Arrow (TyVar 0) (TyVar 0)
    term = (ELam (PVar "x") (EVar "x"))

infer2incr :: Test
infer2incr =
  TestCase $ do
    case runInfer term of
      Left e -> assertFailure ("No type: " ++ show e)
      Right tyAns -> assertEqual "" ty tyAns
  where
    ty = Arrow (TyVar 0) (TyVar 0)
    term = ELam (PVar "x") (EApp (EApp (EVar "+") x) x)
    x = EVar "x"

inferTests :: Test
inferTests = TestList [infer1id, infer2incr]

tests :: Test
tests = TestList [TestLabel "InferTests" inferTests]
