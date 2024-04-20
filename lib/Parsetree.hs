{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE InstanceSigs #-}
module Parsetree where

import Data.List
import Data.Set
import Debug.Trace

data PrimType =
  IntType
  | BoolType
  | UnitType
  deriving (Eq, Show)

newtype TypeIdent = TypeIdent Int deriving (Eq, Ord, Show)

intOfIdent :: TypeIdent -> Int
intOfIdent (TypeIdent n) = n

infixr 3 :->

data Ty
  = Prm PrimType
  | TyVar TypeIdent
  | Ty :-> Ty
  | TApp Ty Ty
  deriving (Eq)

tint :: Ty
tint = Prm IntType

tbool :: Ty
tbool = Prm BoolType

tunit :: Ty
tunit = Prm UnitType

tvar :: TypeIdent -> Ty
tvar = TyVar

tarrow :: Ty -> Ty -> Ty
tarrow = (:->)

tapp :: Ty -> Ty -> Ty
tapp = TApp

instance Show Ty where
  show :: Ty -> String
  show !ty = let rez = helper 3 ty in rez
    where
    helper 0 _ = "FUCK"
    helper n _ | n < 0 = undefined
    helper _ (Prm s) = show s
    helper _ (TyVar v) = "'" ++ show v
    helper n (left :-> right) = "(" ++ helper (n - 1) left ++ " -> " ++ helper (n - 1) right ++ ")"

data Const
  = ConstInt Int
  | ConstBool Bool
  | ConstUnit
  deriving (Show, Eq)

data RecFlag
  = Recursive
  | NonRecursive
  deriving (Show, Eq)

----------------------- Pattern --------------------------

data PatternModifier
  = PMUnique
  | PMLocalExclusive
  | PMOnce
  | PMNone
  deriving (Show, Eq)

pmUnique :: PatternModifier
pmUnique = PMUnique

pmLocal :: PatternModifier
pmLocal = PMUnique

pmOnce :: PatternModifier
pmOnce = PMUnique

pmNone :: PatternModifier
pmNone = PMNone

-- Text as Ident
type Ident = String

data Pattern
  = PVar PatternModifier Ident
  | PAscr PatternModifier Ident Ty
  | PUnit
  | PAny
  deriving (Show, Eq)

pvar :: PatternModifier -> Ident -> Pattern
pvar = PVar

pascr :: PatternModifier -> Ident -> Ty -> Pattern
pascr = PAscr

punit :: Pattern
punit = PUnit

pany :: Pattern
pany = PAny

data Expr
  = EConst Const
  | EVar String
  | EBorrow Expr
  | EIf Expr Expr Expr
  | ELam Pattern Expr
  | EApp Expr Expr
  | ELet RecFlag Pattern Expr Expr
  deriving (Show, Eq)

occurs_in :: TypeIdent -> Ty -> Bool
occurs_in = helper
 where
  helper v (TyVar !x) = x == v
  helper v (!l :-> !r) = helper v l || helper v r
  helper v (Prm _) = False

free_vars :: Ty -> Set TypeIdent
free_vars = helper empty
 where
  helper acc (TyVar v) = Data.Set.insert v acc
  helper acc (Prm _) = acc
  helper acc (l :-> r) = helper (helper acc r) l

-------------------------- Constructors ----------------------------

intConst :: Int -> Expr
intConst = EConst . ConstInt

boolConst :: Bool -> Expr
boolConst = EConst . ConstBool

unitConst :: Expr
unitConst = EConst ConstUnit

recOfBool :: Bool -> RecFlag
recOfBool f
    | f = Recursive
    | otherwise = NonRecursive

let_ :: Bool -> Pattern -> [Pattern] -> Expr -> Expr -> Expr
let_ isRec pat@(PVar PMNone _) args@(_ : _) expr body =
  let expr' = lams args expr
  in ELet (recOfBool isRec) pat expr' body
let_ isRec pat [] expr body = ELet (recOfBool isRec) pat expr body
let_ _ _ _ _ _ = error "let parsing failed"

var :: Ident -> Expr
var = EVar

borrow :: Expr -> Expr
borrow = EBorrow

if_ :: Expr -> Expr -> Expr -> Expr
if_ = EIf

lam :: Pattern -> Expr -> Expr
lam = ELam

app :: Expr -> Expr -> Expr
app = EApp

lams :: [Pattern] -> Expr -> Expr
lams xs e = Data.List.foldr lam e xs