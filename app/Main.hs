module Main where

import Inferencer
import Parsetree
import Typedtree

main :: IO ()
main = do
  putStrLn "hello"
  case runInfer term of
    Left e -> putStrLn ("No type: " ++ show e)
    Right tyAns -> print tyAns
  where
    ty = Arrow (TyVar 0) (TyVar 0)
    term = ELam (PVar "x") (EApp (EApp (EVar "+") x) x)
    x = EVar "x"