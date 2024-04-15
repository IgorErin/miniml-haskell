{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Typedtree where

import Data.Set
import Debug.Trace

data Error = OccursCheck | NoVariable String | UnificationFailed Ty Ty
  deriving (Eq, Show)

data Ty
  = Prm String
  | TyVar Int
  | Arrow Ty Ty
  deriving (Eq, Show)

occurs_in :: Int -> Ty -> Bool
occurs_in v ty =
  trace ("Occurs " ++ show v ++ "in " ++ show ty ++ "?") $ helper v ty
  where
    helper v (TyVar !x) = trace "OCCURS1" $ x == v
    helper v (Arrow !l !r) = trace "OCCURS1" $ helper v l || helper v r
    helper v (Prm _) = trace "OCCURS1" $ False

free_vars :: Ty -> Set Int
free_vars = helper empty
  where
    helper acc (TyVar v) = insert v acc
    helper acc (Prm _) = acc
    helper acc (Arrow l r) = helper (helper acc r) l
