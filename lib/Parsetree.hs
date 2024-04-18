{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Avoid lambda" #-}
module Parsetree where

import Data.List
import Data.Set
import Debug.Trace

data Ty
  = Prm String
  | TyVar Int
  | Arrow Ty Ty
  deriving (Eq)

instance Show Ty where
  show !ty = let rez = helper 3 ty in rez
   where
    helper 0 _ = "FUCK"
    helper n _ | n < 0 = undefined
    helper _ (Prm s) = s
    helper _ (TyVar v) = "'" ++ show v
    helper n (Arrow left right) = "(" ++ helper (n - 1) left ++ " -> " ++ helper (n - 1) right ++ ")"

data Const
  = PConst_int Int
  | PConst_bool Bool
  deriving (Show, Eq)

data RecFlag
  = Recursive
  | NonRecursive
  deriving (Show, Eq)

data PatternModifier
  = PMUnique
  | PMLocalExclusive
  | PMNone
  deriving (Show, Eq)

data Pattern
  = PVar String PatternModifier
  | PAscr String Ty
  | PUnit
  | PAny
  deriving (Show, Eq)

pvar s = PVar s PMNone

data Expr
  = EConst Const
  | EVar String
  | EBorrow Expr
  | EIf Expr Expr Expr
  | ELam Pattern Expr
  | EApp Expr Expr
  | ELet RecFlag Pattern Expr Expr
  deriving (Show, Eq)

econst_int n = EConst (PConst_int n)

elams :: [Pattern] -> Expr -> Expr
elams xs e = Data.List.foldr ELam e xs

evar = EVar

data StructureItem = SItem RecFlag Pattern Expr deriving (Show, Eq)

newtype Program = Program [StructureItem] deriving (Show, Eq)

(@->) = Arrow

occurs_in :: Int -> Ty -> Bool
occurs_in = helper
 where
  helper v (TyVar !x) = x == v
  helper v (Arrow !l !r) = helper v l || helper v r
  helper v (Prm _) = False

free_vars :: Ty -> Set Int
free_vars = helper empty
 where
  helper acc (TyVar v) = Data.Set.insert v acc
  helper acc (Prm _) = acc
  helper acc (Arrow l r) = helper (helper acc r) l
