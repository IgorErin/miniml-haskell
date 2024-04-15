module Scheme where

import Data.Set
import Subst
import Typedtree

data Scheme = Scheme (Set Int) Ty

occurs_in :: Int -> Scheme -> Bool
occurs_in = undefined

free_vars :: Scheme -> Set Int
free_vars = undefined

apply :: Subst -> Scheme -> Scheme
apply _ _ = undefined

ofTy :: Ty -> Scheme
ofTy ty = Scheme (Typedtree.free_vars ty) ty