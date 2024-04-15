{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Avoid lambda" #-}
module Scheme where

import Data.Set
import Subst
import Typedtree

data Scheme = Scheme (Set Int) Ty

occurs_in :: Int -> Scheme -> Bool
occurs_in v (Scheme names ty) =
  not (Data.Set.member v names) && Typedtree.occurs_in v ty

free_vars :: Scheme -> Set Int
free_vars (Scheme names ty) = Data.Set.difference (Typedtree.free_vars ty) names

apply :: Subst -> Scheme -> Scheme
apply sub (Scheme names ty) =
  let s2 = Data.Set.foldr (\k acc -> Subst.remove acc k) sub names
   in Scheme names (Subst.apply s2 ty)

ofTy :: Ty -> Scheme
ofTy ty = Scheme (Typedtree.free_vars ty) ty