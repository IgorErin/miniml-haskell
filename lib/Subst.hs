{-# LANGUAGE NoImplicitPrelude #-}

module Subst where

import Data.Map
import Typedtree
import Prelude (Either, Int, Maybe (Just, Nothing), undefined)

newtype Subst = Subst (Map Int Ty)

empty :: Subst
empty = Subst Data.Map.empty

singleton :: Int -> Ty -> Subst
singleton k v = Subst (fromList [(k, v)])

find :: Int -> Subst -> Maybe Ty
find k (Subst s) = lookup k s

apply :: Subst -> Ty -> Ty
apply (Subst s) ty@(TyVar b) =
  case Data.Map.lookup b s of
    Just x -> x
    Nothing -> ty
apply s (Arrow l r) = Arrow (apply s l) (apply s r)

unify :: Ty -> Ty -> Either Error Subst
unify = undefined

compose :: Subst -> Subst -> Either Error Subst
compose = undefined

compose_all :: [Subst] -> Either Error Subst
compose_all = undefined

remove :: Subst -> Int -> Subst
remove = undefined