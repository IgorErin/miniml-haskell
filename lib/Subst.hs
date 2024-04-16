{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE StrictData #-}
-- {-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas -Wincomplete-patterns  -fwarn-name-shadowing #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns -fwarn-incomplete-patterns #-}

{-# HLINT ignore "Use camelCase" #-}

module Subst where

import Control.Arrow (Arrow, ArrowApply (app))
import Control.Monad ((>>=))
import Data.Foldable (foldlM)
import Data.Maybe (fromMaybe)
import qualified Data.Set
import Data.Strict.List
import Debug.Trace
import System.IO.Unsafe
import Typedtree
import Prelude hiding (lookup)

-- import Prelude (Either (Left, Right), Eq, Int, Maybe (Just, Nothing), Monoid (mappend), Show, foldl, otherwise, putStrLn, return, show, undefined, ($), (++), (==))

-- import qualified Data.Map.Strict as IntMap

newtype Subst = Subst (List (Int, Ty)) deriving (Show, Eq)

ofList :: [(Int, Ty)] -> Subst
ofList xs = Subst $ foldl (flip (:!)) Nil xs -- TODO: check keys are unique

empty :: Subst
empty = Subst Nil
cons h tl = h :! tl

singleton :: Int -> Ty -> Subst
singleton !k !v = Subst $ (k, v) :! Nil

lookup k Nil = Nothing
lookup k ((k2, v) :! _) | k == k2 = Just v
lookup k (_ :! tl) = lookup k tl

find :: Int -> Subst -> Maybe Ty
find k (Subst s) = lookup k s

apply :: Subst -> Ty -> Ty
apply ss t =
  -- trace ("apply: " ++ show ss ++ " " ++ show t) $
  helper ss t
 where
  helper (Subst !su) ty@(TyVar !b) = fromMaybe ty (lookup b su)
  helper !su (Arrow !l !r) =
    -- trace "apply2" $
    Arrow (helper su l) (helper su r)
  helper _ x@(Prm _) = x

unify :: Ty -> Ty -> Either Error Subst
unify !left !right =
  -- let rez = trace ("In unification " ++ show left ++ " and " ++ show right) $ helper left right
  --  in trace ("Unify " ++ show left ++ "and " ++ show right ++ " = " ++ show rez) rez
  helper left right
 where
  helper (Prm l) (Prm r) | l == r = return empty
  helper l@(Prm _) r@(Prm _) = Left (UnificationFailed l r)
  helper (TyVar l) (TyVar r) | l == r = return empty
  helper (TyVar l) r | Typedtree.occurs_in l r = Left OccursCheck
  helper (TyVar l) r = return $ singleton l r
  helper r (TyVar l) | Typedtree.occurs_in l r = Left OccursCheck
  helper r (TyVar l) = return $ singleton l r
  helper (Arrow l1 r1) (Arrow l2 r2) = do
    subs1 <- helper l1 l2
    subs2 <- helper (apply subs1 r1) (apply subs1 r2)
    compose subs1 subs2
  helper l@(Arrow _ _) r@(Prm _) = Left $ UnificationFailed l r
  helper l@(Prm _) r@(Arrow _ _) = Left $ UnificationFailed l r

mapping !k !v =
  if Typedtree.occurs_in k v
    then Left OccursCheck
    else Right (k, v)

extend :: Subst -> (Int, Ty) -> Either Error Subst
extend (Subst !s) (k, v) =
  -- trace (show (k, v, s)) $
  --   trace ("IntMap.lookup k s = " ++ show (lookup k s)) $
  case lookup k s of
    Nothing ->
      let !s2 = singleton k (apply (Subst s) v)
       in do
            foldlM
              ( \(Subst acc) (k2, v2) ->
                  -- trace ("asdf:" ++ show acc ++ "s2 = " ++ show s2) $
                  --   trace ("v = " ++ show v) $
                  do
                    m <- mapping k2 (apply s2 v2)
                    return $ Subst (cons m acc)
              )
              s2
              s
    Just v2 -> do
      s2 <- unify v v2
      compose (Subst s) s2

compose :: Subst -> Subst -> Either Error Subst
compose (Subst !s1) (Subst !s2) =
  -- trace ("\nCompose: " ++ show s1 ++ " and " ++ show s2 ++ "\n") $
  foldlM extend (Subst s1) s2

compose_all :: [Subst] -> Either Error Subst
compose_all = foldlM compose empty

remove :: Subst -> Int -> Subst
remove = undefined