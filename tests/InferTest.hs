module InferTest where

import Inferencer
import Parsetree
import Subst
import Test.HUnit
import Typedtree

infer0compose :: Test
infer0compose =
  TestCase $ do
    case Subst.compose (Subst.singleton 2 (Prm "int")) (Subst.singleton 3 (Prm "int")) of
      Left e -> assertFailure ("No type: " ++ show e)
      Right tyAns -> assertEqual "" ty tyAns
 where
  ty = Subst.ofList [(2, Prm "int"), (3, Prm "int")]

infer0extend :: Test
infer0extend =
  TestCase $ do
    case Subst.extend (Subst.singleton 2 (Prm "int")) (3, Prm "int") of
      Left e -> assertFailure ("No type: " ++ show e)
      Right tyAns -> assertEqual "" ty tyAns
 where
  ty = Subst.ofList [(3, Prm "int"), (2, Prm "int")]

infer1id :: Test
infer1id =
  TestCase $ do
    case runInfer term of
      Left e -> assertFailure ("No type for '" ++ show term ++ ": " ++ show e)
      Right tyAns -> assertEqual "" ty tyAns
 where
  ty = Arrow (TyVar 0) (TyVar 0)
  term = ELam (PVar "x") (EVar "x")

infer2part1 :: Test
infer2part1 =
  TestCase $ do
    case runInfer term of
      Left e -> assertFailure ("No type for '" ++ show term ++ "':  " ++ show e)
      Right tyAns -> assertEqual "" ty tyAns
 where
  ty = Arrow (Prm "int") (Prm "int")
  term = EApp (EVar "+") (EConst 1)

infer3twice :: Test
infer3twice =
  TestCase $ do
    case runInfer term of
      Left e -> assertFailure ("No type for '" ++ show term ++ "':  " ++ show e)
      Right tyAns -> assertEqual "" ty tyAns
 where
  ty = Arrow (Prm "int") (Prm "int")
  term = ELam (PVar "x") (EApp (EApp (EVar "+") x) x)
  x = EVar "x"

infer4fix :: Test
infer4fix =
  TestCase $ do
    case runInfer term of
      Left e -> assertFailure ("No type for '" ++ show term ++ "':  " ++ show e)
      Right tyAns -> assertEqual "" ty tyAns
 where
  ty = Arrow (Prm "int") (Prm "int")
  term =
    ELet
      Recursive
      (PVar "fix")
      (ELam (PVar "f") (EApp (EVar "f") (EApp (EVar "fix") (EVar "f"))))
      (EVar "fix")
  x = EVar "x"

inferTests :: Test
inferTests =
  TestList
    [ infer0extend
    , infer1id
    , infer2part1
    , infer3twice
    , infer4fix
    ]

tests :: Test
tests = TestList [TestLabel "InferTests" inferTests]
