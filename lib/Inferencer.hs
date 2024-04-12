module Inferencer where

import Parsetree
import Typedtree

data Error = OccursCheck | NoVariable String | UnificationFailed Ty Ty

newtype Infer a = Infer (Int -> (Int, Either Error a))

instance Functor Infer where
  fmap f (Infer i) = Infer (\a -> map2 (fmap f) (i a))
   where
    map2 f (a, b) = (a, f b)

instance Applicative Infer where
  pure a = Infer $ \x -> (x, Right a)
  Infer fa <*> Infer farg =
    Infer
      ( \st ->
          let (st0, rhs) = fa st
           in case rhs of
                Left e -> Left e
                Right x ->
                  let (st1, rez) = farg st0
                   in (st1, Right rez)
      )
instance Monad Infer where
  return a = Infer $ \x -> (x, Right a)

basicSum :: Int -> Int -> Int
basicSum x y = x + y

--