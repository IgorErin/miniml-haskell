{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns -fwarn-incomplete-patterns #-}

module Subst where

import Control.Arrow (Arrow, ArrowApply (app))
import Control.Monad ((>>=))
import qualified Data.Map.Strict as IntMap
import Data.Maybe (fromMaybe)
import qualified Data.Set
import Debug.Trace
import System.IO.Unsafe
import Typedtree
import Prelude (Either (Left, Right), Int, Maybe (Just, Nothing), Monoid (mappend), Show, otherwise, putStrLn, return, show, undefined, ($), (++), (==))

newtype Subst = Subst (IntMap.Map Int Ty) deriving (Show)

empty :: Subst
empty = Subst IntMap.empty

singleton :: Int -> Ty -> Subst
singleton !k !v = Subst (IntMap.fromList [(k, v)])

find :: Int -> Subst -> Maybe Ty
find k (Subst s) = IntMap.lookup k s

apply :: Subst -> Ty -> Ty
apply (Subst !s) ty@(TyVar !b) =
  trace "apply1" $
    fromMaybe ty (IntMap.lookup b s)
apply !s (Arrow !l !r) =
  trace "apply2" $
    Arrow (apply s l) (apply s r)
apply _ !x = trace "apply3" $ x

unify :: Ty -> Ty -> Either Error Subst
unify (Prm l) (Prm r) | l == r = return empty
unify l@(Prm _) r@(Prm _) = Left (UnificationFailed l r)
unify (TyVar l) (TyVar r) | l == r = return empty
unify (TyVar l) r = return $ singleton l r
unify r (TyVar l) = return $ singleton l r
unify (Arrow l1 r1) (Arrow l2 r2) = do
  subs1 <- unify l1 l2
  subs2 <- unify (apply subs1 l1) (apply subs1 r2)
  compose subs1 subs2
unify l@(Arrow _ _) r@(Prm _) = Left $ UnificationFailed l r
unify l@(Prm _) r@(Arrow _ _) = Left $ UnificationFailed l r

mapping !k !v =
  trace "mapping" $
    if Typedtree.occurs_in k v
      then Left OccursCheck
      else Right (k, v)

compose :: Subst -> Subst -> Either Error Subst
compose (Subst !s1) (Subst !s2) =
  trace ("\nCompose: " ++ show s1 ++ " and " ++ show s2 ++ "\n") $
    IntMap.foldrWithKey' extend (return (Subst s1)) s2
  where
    extend !k !v !s =
      s >>= \s1@(Subst !s) ->
        trace (show (k, v, s)) $
          trace ("IntMap.lookup k s = " ++ show (IntMap.lookup k s)) $
            case IntMap.lookup k s of
              Nothing ->
                let !v = (trace ("Go apply:   v = " ++ show v) $ apply s1 v)
                 in let !s2 = trace "Go singleton" $ singleton k v
                     in do
                          IntMap.foldlWithKey'
                            ( \acc k v ->
                                let !v = apply s2 v
                                 in do
                                      Subst acc <- acc
                                      (k, v) <- mapping k v

                                      return $
                                        Subst
                                          (trace "HERR4" $ IntMap.insert k v acc)
                            )
                            (return s2)
                            s
              Just v2 -> trace "HERR3" $
                do
                  s2 <- unify v v2
                  compose s1 s2

compose_all :: [Subst] -> Either Error Subst
compose_all = undefined

remove :: Subst -> Int -> Subst
remove = undefined