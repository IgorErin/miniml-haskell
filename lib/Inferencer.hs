{-# LANGUAGE CPP #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wincomplete-patterns  -fwarn-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use record patterns" #-}

module Inferencer where

import Data.Char (generalCategory)
import Data.Map
import Data.Set
import Debug.Trace
import Parsetree
import Scheme
import qualified Subst (Subst, apply, compose, compose_all, empty, singleton, unify)
import Typedtree
import Prelude hiding (fail)

newtype Infer a = Infer (Int -> (Int, Either Error a))

instance Functor Infer where
  fmap f (Infer i) = Infer (\a -> map2 (fmap f) (i a))
    where
      map2 foo (a, b) = (a, foo b)

instance Applicative Infer where
  pure a = Infer $ \x -> (x, Right a)
  (<*>) :: Infer (a -> b) -> Infer a -> Infer b
  Infer fa <*> Infer farg =
    Infer
      ( \st ->
          let (st0, f2) = fa st
              (st1, arg2) = farg st0
           in case (f2, arg2) of
                (Right f, Right arg) -> (st1, Right (f arg))
      )

instance Monad Infer where
  (>>=) (Infer m) f = Infer $ \st ->
    let (st0, x) = m st
     in case x of
          Right rez -> let (Infer rhs) = f rez in rhs st0
          Left e -> (st0, Left e)

fail :: Error -> Infer a
fail e = Infer (\st -> (st, Left e))

fresh :: Infer Int
fresh = Infer (\st -> (st + 1, Right st))

freshVar :: Infer Ty
freshVar = fmap TyVar fresh

instantiate :: Scheme -> Infer Ty
instantiate (Scheme vars t) =
  Data.Set.foldr f (return t) vars
  where
    f name ty = do
      f1 <- freshVar
      typ <- ty
      let s = Subst.singleton name f1
      return (Subst.apply s typ)

newtype Env = Env (Map String Scheme) deriving (Show)

freeVarsEnv :: Env -> Data.Set.Set Int
freeVarsEnv (Env env) =
  Data.Map.foldl (\acc v -> Data.Set.union acc (Scheme.free_vars v)) Data.Set.empty env

defaultEnv :: Env
defaultEnv =
  Env $
    Data.Map.fromList
      [ ("*", arith),
        ("/", arith),
        ("-", arith),
        ("+", arith),
        ("=", eqS)
      ]
  where
    arith = Scheme.ofTy $ Arrow (Prm "int") $ Arrow (Prm "int") (Prm "int")
    eqS = Scheme.ofTy $ Arrow (TyVar 0) $ Arrow (TyVar 0) $ Prm "bool"

lookupEnv :: Env -> String -> Infer (Subst.Subst, Ty)
lookupEnv (Env env) name =
  case Data.Map.lookup name env of
    Nothing -> fail (NoVariable name)
    Just scheme -> do
      ans <- instantiate scheme
      return (Subst.empty, ans)

extendEnv k v (Env m) = Env (Data.Map.insert k v m)

applyEnv s1 (Env m) =
  Env (fmap (Scheme.apply s1) m)

inferOfEither :: Either Error e -> Infer e
inferOfEither (Left e) = fail e
inferOfEither (Right x) = return x

unify :: Ty -> Ty -> Infer Subst.Subst
unify a b = inferOfEither $ Subst.unify a b

generalize :: Env -> Ty -> Scheme
generalize env ty = Scheme free ty
  where
    free = Data.Set.difference (Typedtree.free_vars ty) (freeVarsEnv env)

infer :: Env -> Parsetree.Expr -> Infer (Subst.Subst, Ty)
infer env (EConst (PConst_int _)) = return (Subst.empty, Prm "int")
infer env (EConst (PConst_bool _)) = return (Subst.empty, Prm "bool")
infer env (EVar x) = lookupEnv env x
infer env (ELam (PVar x) e1) = do
  tv <- freshVar
  let env2 = extendEnv x (Scheme Data.Set.empty tv) env
  (s, ty) <- infer env2 e1
  let trez = Arrow (Subst.apply s tv) ty
  return (s, trez)
infer env (EApp e1 e2) = do
  (s1, t1) <- infer env e1
  (s2, t2) <- infer (applyEnv s1 env) e2
  tv <- freshVar
  s3 <- unify (Subst.apply s2 t1) (Arrow t2 tv)
  let trez = Subst.apply s3 tv
  final <- inferOfEither $ Subst.compose_all [s3, s2, s1]
  return (final, trez)
infer env (EIf c th el) = do
  (s1, t1) <- infer env c
  (s2, t2) <- infer env th
  (s3, t3) <- infer env el
  s4 <- unify t1 (Prm "bool")
  s5 <- unify t2 t3
  final_subst <- inferOfEither $ Subst.compose_all [s5, s4, s3, s2, s1]
  return (final_subst, Subst.apply s5 t2)
infer env (ELet NonRecursive (PVar x) e1 e2) = do
  (s1, t1) <- infer env e1
  let env2 = (applyEnv s1 env)
  let t2 = generalize env2 t1
  (s2, t3) <- infer (extendEnv x t2 env2) e2
  final_subst <- inferOfEither $ Subst.compose s1 s2
  return (final_subst, t3)
infer env (ELet Recursive (PVar x) e1 e2) = do
  tv <- freshVar
  let env2 = extendEnv x (Scheme Data.Set.empty tv) env
  (s1, t1) <- infer env2 e1
  s2 <- unify (Subst.apply s1 tv) t1
  s <- inferOfEither $ Subst.compose s2 s1
  let env3 = applyEnv s env2
  let t2 = generalize env3 (Subst.apply s tv)
  (s3, t3) <- infer (extendEnv x t2 (applyEnv s env)) e2
  final_subst <- inferOfEither $ Subst.compose s s3
  return (final_subst, t3)

runInfer :: Parsetree.Expr -> Either Error Ty
runInfer expr =
  let (Infer f) = infer defaultEnv expr
   in case snd $ f 0 of
        Left err -> Left err
        Right (_, ty) -> Right ty

basicSum :: Int -> Int -> Int
basicSum x y = x + y
