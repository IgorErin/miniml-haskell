{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use record patterns" #-}

module Inferencer where

import Data.Map
import Data.Set
import Debug.Trace
import Parsetree
import Scheme
import qualified Subst (Subst, apply, compose_all, empty, singleton, unify)
import Typedtree
import Prelude hiding (fail)

newtype Infer a = Infer (Int -> (Int, Either Error a))

instance Functor Infer where
  fmap f (Infer i) = Infer (\a -> map2 (fmap f) (i a))
    where
      map2 f (a, b) = (a, f b)

instance Applicative Infer where
  pure a = Infer $ \x -> (x, Right a)
  (<*>) :: Infer (a -> b) -> Infer a -> Infer b
  Infer fa <*> Infer farg =
    Infer
      ( \st ->
          let (st0, f) = fa st
              (st1, arg) = farg st0
           in case (f, arg) of
                (Right f, Right arg) -> (st1, Right (f arg))
      )

instance Monad Infer where
  (>>=) (Infer m) f = Infer $ \st ->
    let (st0, x) = m st
     in case x of
          Right x -> let (Infer rhs) = f x in rhs st0
          Left e -> (st0, Left e)

fail :: Error -> Infer a
fail e = Infer (\st -> (st, Left e))

fresh :: Infer Int
fresh = Infer (\st -> (st + 1, Right st))

freshVar :: Infer Ty
freshVar = fmap TyVar fresh

instantiate :: Scheme -> Infer Ty
instantiate (Scheme vars ty) =
  Data.Set.foldr f (return ty) vars
  where
    f name ty = do
      f1 <- freshVar
      typ <- ty
      let s = Subst.singleton name f1
      return (Subst.apply s typ)

newtype Env = Env (Map String Scheme)

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
    arith = Scheme.ofTy $ Arrow (Prm "int") $ Arrow (Prm "int") $ Prm "int"
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

infer :: Env -> Parsetree.Expr -> Infer (Subst.Subst, Ty)
infer env (EConst _) = return (Subst.empty, Prm "int")
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

  final <- inferOfEither $ trace "HERRE" $ Subst.compose_all [s3, s2, s1]

  return (final, trez)
infer env (EIf _ _ _) = undefined
infer env (ELet _ _ _ _) = undefined

runInfer :: Parsetree.Expr -> Either Error Ty
runInfer e =
  let (Infer f) = infer defaultEnv e
   in case snd $ f 0 of
        Left e -> Left e
        Right (_, ty) -> Right ty

basicSum :: Int -> Int -> Int
basicSum x y = x + y
