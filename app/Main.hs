module Main where

import Inferencer
import Parsetree
import Subst
import Typedtree

main :: IO ()
main = do
  putStrLn "hello"
  case Subst.extend (Subst.singleton 2 (Prm "int")) (3, (Prm "int")) of
    Left e -> putStrLn ("No type: " ++ show e)
    Right tyAns -> print tyAns
  where
    --   print $ compose (Subst.singleton 0 (Prm "int")) (Subst.singleton 1 (Prm "int"))

    --   print $ apply (Subst.singleton 0 (Prm "int")) ((Prm "int"))

    ty = Arrow (TyVar 0) (TyVar 0)
    term = ELam (PVar "x") (EApp (EApp (EVar "+") x) x)
    x = EVar "x"