module Main where

import Inferencer
import Parsetree
import Subst
import Typedtree

-- main :: IO ()
-- main = do
--   putStrLn "hello"
--   case Subst.extend (Subst.singleton 2 (Prm "int")) (3, Prm "int") of
--     Left e -> putStrLn ("No type: " ++ show e)
--     Right tyAns -> print tyAns
--  where
--   --   print $ compose (Subst.singleton 0 (Prm "int")) (Subst.singleton 1 (Prm "int"))

--   --   print $ apply (Subst.singleton 0 (Prm "int")) ((Prm "int"))

--   ty = Arrow (TyVar 0) (TyVar 0)
--   term = ELam (PVar "x") (EApp (EApp (EVar "+") x) x)
--   x = EVar "x"
main :: IO ()
main = do
  print $ Subst.unify t2 t1
 where
  t1 = Arrow (TyVar 0) (TyVar 1)
  t2 = TyVar 0

-- main :: IO ()
-- main = do
--   print $ Subst.unify t1 t2
--  where
--   t1 = Arrow (TyVar 0) (TyVar 1)
--   t2 = TyVar 0

-- main :: IO ()
-- main = do
--   print $ runInfer term
--  where
--   ty = Arrow (TyVar 0) (TyVar 0)
--   term = ELam (PVar "x") (EApp x (EVar "x"))
--   x = EVar "x"
