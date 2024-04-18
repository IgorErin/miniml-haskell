{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Typedtree where

import Parsetree

data Error = OccursCheck | NoVariable String | UnificationFailed Ty Ty
  deriving (Eq, Show)

-- instance Show Error where
--   show _ = "FUCK"

