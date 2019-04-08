module Data.Char.Properties.Misc
  ( module Data.Char.Properties.MiscData
  , module Data.Char.Properties.Derivation
  , module Data.Char.Properties.Misc
  ) where

import Data.Char.Properties.Derivation
import Data.Char.Properties.GeneralCategory
import Data.Char.Properties.MiscData
import Prelude
        -- | Returns true if the general category is Lt.

isTitlecase :: Char -> Bool
isTitlecase c = getGeneralCategory c == GcLt
        -- Note: this is not defined by Unicode.
        -- Also, a single line break can be an 0D 0A sequence.

isLineBreak :: Char -> Bool
isLineBreak '\x000A' = True -- LINE FEED
isLineBreak '\x000B' = True -- VERTICAL TABULATION ?
isLineBreak '\x000C' = True -- FORM FEED
isLineBreak '\x000D' = True -- CARRIAGE RETURN
isLineBreak '\x2028' = True -- LINE SEPARATOR
isLineBreak '\x2029' = True -- PARAGRAPH SEPARATOR
isLineBreak _ = False
