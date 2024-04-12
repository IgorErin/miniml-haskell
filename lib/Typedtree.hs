module Typedtree where

data Ty
    = Prm String
    | TyVar Int
    | Arrow Ty Ty