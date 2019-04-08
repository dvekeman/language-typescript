module Data.Char.Properties.DecompositionTypes where

import Prelude

data DecompositionType
  = DCCanonical
  | DCFont
  | DCNoBreak
  | DCInitial
  | DCMedial
  | DCFinal
  | DCIsolated
  | DCCircle
  | DCSuper
  | DCSub
  | DCVertical
  | DCWide
  | DCNarrow
  | DCSmall
  | DCSquare
  | DCFraction
  | DCCompat
  deriving (Eq)

data Decomposition
  = NoDecomposition
  | MkDecomposition DecompositionType
                    [Char]
  deriving (Eq)
