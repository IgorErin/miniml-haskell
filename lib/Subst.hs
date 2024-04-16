{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas -Wincomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns -fwarn-incomplete-patterns #-}

{-# HLINT ignore "Use camelCase" #-}

module Subst where

import Control.Arrow (Arrow, ArrowApply (app))
import Control.Monad ((>>=))
-- import qualified Data.Map.Strict as IntMap

import Data.Foldable (foldlM)
import Data.Maybe (fromMaybe)
import qualified Data.Set
import Debug.Trace
import System.IO.Unsafe
import Typedtree
import Prelude (Either (Left, Right), Eq, Int, Maybe (Just, Nothing), Monoid (mappend), Show, lookup, otherwise, putStrLn, return, show, undefined, ($), (++), (==))

newtype Subst = Subst [(Int, Ty)] deriving (Show, Eq)

empty :: Subst
empty = Subst []

singleton :: Int -> Ty -> Subst
singleton !k !v = Subst [(k, v)]

find :: Int -> Subst -> Maybe Ty
find k (Subst s) = lookup k s

apply :: Subst -> Ty -> Ty
apply s ty =
  trace ("apply: subst = " ++ "?" ++ ", ty = " ++ show ty) $ helper s ty
  where
    helper (Subst !s) ty@(TyVar !b) = fromMaybe ty (lookup b s)
    helper !s (Arrow !l !r) =
      trace "apply2" $
        Arrow (helper s l) (helper s r)
    helper _ x@(Prm _) = x

unify :: Ty -> Ty -> Either Error Subst
unify l r =
  let rez = unify l r
   in trace ("Unify " ++ show l ++ "and " ++ show r ++ " = " ++ show rez) rez
  where
    helper (Prm l) (Prm r) | l == r = return empty
    helper l@(Prm _) r@(Prm _) = Left (UnificationFailed l r)
    helper (TyVar l) (TyVar r) | l == r = return empty
    helper (TyVar l) r = return $ singleton l r
    helper r (TyVar l) = return $ singleton l r
    helper (Arrow l1 r1) (Arrow l2 r2) = do
      subs1 <- helper l1 l2
      subs2 <- helper (apply subs1 l1) (apply subs1 r2)
      compose subs1 subs2
    helper l@(Arrow _ _) r@(Prm _) = Left $ UnificationFailed l r
    helper l@(Prm _) r@(Arrow _ _) = Left $ UnificationFailed l r

mapping !k !v =
  trace "mapping" $
    if Typedtree.occurs_in k v
      then Left OccursCheck
      else Right (k, v)

extend :: Subst -> (Int, Ty) -> Either Error Subst
extend (Subst !s) (!k, !v) =
  trace (show (k, v, s)) $
    trace ("IntMap.lookup k s = " ++ show (lookup k s)) $
      case lookup k s of
        Nothing ->
          --   let !v = (trace ("Go apply:   v = " ++ show v) $ apply (Subst s) v)
          --    in let !s2 = trace "Go singleton" $ singleton k v
          --        in
          let !s2 = trace "Go singleton" $ singleton k v
           in do
                trace "HERRRRR" $
                  foldlM
                    ( \(Subst acc) (k, v) ->
                        trace ("asdf:" ++ show acc ++ "s2 = " ++ show s2) $
                          trace ("v = " ++ show v) $
                            let v = trace "76" $ apply s2 v
                             in do
                                  (k, v) <- trace "78" $ mapping k v
                                  return $ Subst ((k, v) : acc)
                    )
                    s2
                    s
        Just v2 -> trace "HERR3" $
          do
            s2 <- unify v v2
            compose (Subst s) s2

compose :: Subst -> Subst -> Either Error Subst
compose (Subst !s1) (Subst !s2) =
  trace ("\nCompose: " ++ show s1 ++ " and " ++ show s2 ++ "\n") $
    foldlM extend (Subst s1) s2

compose_all :: [Subst] -> Either Error Subst
compose_all = undefined

remove :: Subst -> Int -> Subst
remove = undefined