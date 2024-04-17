{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Typedtree where

import Data.Set
import Debug.Trace

data Error = OccursCheck | NoVariable String | UnificationFailed Ty Ty
  deriving (Eq, Show)

-- instance Show Error where
--   show _ = "FUCK"

data Ty
  = Prm !String
  | TyVar !Int
  | Arrow !Ty !Ty
  deriving (Eq)

instance Show Ty where
  show !ty = let rez = helper 3 ty in rez
    where
      helper 0 _ = "FUCK"
      helper n _ | n < 0 = undefined
      helper _ (Prm s) = s
      helper _ (TyVar v) = "'" ++ show v
      helper n (Arrow left right) = "(" ++ helper (n - 1) left ++ " -> " ++ helper (n - 1) right ++ ")"

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
    helper acc (TyVar v) = insert v acc
    helper acc (Prm _) = acc
    helper acc (Arrow l r) = helper (helper acc r) l
