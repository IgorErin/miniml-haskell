module Typedtree where

import Data.Set

data Error = OccursCheck | NoVariable String | UnificationFailed Ty Ty
  deriving (Eq, Show)

data Ty
  = Prm String
  | TyVar Int
  | Arrow Ty Ty
  deriving (Eq, Show)

occurs_in :: Int -> Ty -> Bool
occurs_in v (TyVar x) = (x == v)
occurs_in v (Arrow l r) = occurs_in v l || occurs_in v r
occurs_in v (Prm _) = False

free_vars :: Ty -> Set Int
free_vars = helper empty
  where
    helper acc (TyVar v) = insert v acc
    helper acc (Prm _) = acc
    helper acc (Arrow l r) = helper (helper acc r) l
