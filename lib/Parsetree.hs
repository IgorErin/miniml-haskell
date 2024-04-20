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
  deriving (Eq)

instance Show PrimType where
  show :: PrimType -> String
  show IntType = "int"
  show BoolType = "bool"
  show UnitType = "unit"

newtype TypeIdent = TypeIdent Int deriving (Eq, Ord)

instance Show TypeIdent where
  show :: TypeIdent -> String
  show (TypeIdent n) = show n

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
  deriving (Eq)

instance Show Const where
  show :: Const -> String
  show (ConstInt n) = show n
  show (ConstBool b) = show b
  show ConstUnit = "()"

data RecFlag
  = Recursive
  | NonRecursive
  deriving (Eq)

instance Show RecFlag where
  show :: RecFlag -> String
  show Recursive = "rec"
  show NonRecursive = ""

----------------------- Pattern --------------------------

data PatternModifier
  = PMUnique
  | PMLocalExclusive
  | PMOnce
  | PMNone
  deriving (Eq)

instance Show PatternModifier where
  show :: PatternModifier -> String
  show PMUnique = "mut"
  show PMLocalExclusive = "local"
  show PMOnce = "once"
  show PMNone = ""

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
  deriving (Eq)

instance Show Pattern where
  show :: Pattern -> String
  show (PVar PMNone i) = i
  show (PVar m i) =  "(" ++ show m ++ " " ++ i ++ ")"
  show (PAscr m i t) = "(" ++ show m ++ " " ++ i ++ ": " ++ show t ++ ")"
  show PUnit = "()"
  show PAny = "_"

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
  | EVar Ident
  | EBorrow Expr
  | EIf Expr Expr Expr
  | ELam Pattern Expr
  | EApp Expr Expr
  | ELet RecFlag Pattern Expr Expr
  deriving (Eq)

instance Show Expr where
  show :: Expr -> String
  show (EConst c) = show c
  show (EVar n) = n
  show (EBorrow e) = " &" ++ show e
  show (EIf g t f) = "if " ++ show g ++ "\nthen" ++ show t ++ "\nelse" ++ show f
  show (ELam p e) = "fun" ++ show p ++ " -> " ++ show e
  show (EApp left right) = "(" ++ show left ++ " " ++ show right ++ ")"
  show (ELet recflag pat expr body) =
      let grabArgs (ELam p e) =
            let (ls, b) = grabArgs e
            in (p : ls, b)
          grabArgs e = ([], e)

          (pats, expr') = grabArgs expr

          args = unwords $ show <$> pats
      in "let" ++ show recflag ++ " " ++ show pat ++ " " ++ args ++ " = " ++ show expr' ++ " in\n" ++ show body

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