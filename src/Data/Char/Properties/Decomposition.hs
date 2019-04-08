module Data.Char.Properties.Decomposition
  ( module Data.Char.Properties.DecompositionTypes
  , module Data.Char.Properties.DecompositionData
  , module Data.Char.Properties.Decomposition
  ) where

import Data.Char.Properties.DecompositionData
import Data.Char.Properties.DecompositionTypes
import Prelude

extractDecompositionCanonical :: Decomposition -> Maybe [Char]
extractDecompositionCanonical (MkDecomposition DCCanonical decomp) = Just decomp
extractDecompositionCanonical _ = Nothing

decomposeCanonical :: String -> String
decomposeCanonical [] = []
decomposeCanonical (c:cs) =
  (case (extractDecompositionCanonical (getDecomposition c)) of
     Nothing -> [c]
     Just cd -> decomposeCanonical cd) ++
  (decomposeCanonical cs)

extractDecompositionCompatibility :: Decomposition -> Maybe [Char]
extractDecompositionCompatibility (MkDecomposition _ decomp) = Just decomp
extractDecompositionCompatibility _ = Nothing

decomposeCompatibility :: String -> String
decomposeCompatibility [] = []
decomposeCompatibility (c:cs) =
  (case (extractDecompositionCompatibility (getDecomposition c)) of
     Nothing -> [c]
     Just cd -> decomposeCompatibility cd) ++
  (decomposeCompatibility cs)
