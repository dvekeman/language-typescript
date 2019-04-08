module Data.Char.Properties.GeneralCategory
  ( module Data.Char.Properties.GeneralCategoryTypes
  , module Data.Char.Properties.GeneralCategoryData
  , module Data.Char.Properties.GeneralCategory
  ) where

import Data.Char.Properties.GeneralCategoryData
import Data.Char.Properties.GeneralCategoryTypes
import Prelude

instance Show GeneralCategory where
  show GcLu = "Lu"
  show GcLl = "Ll"
  show GcLt = "Lt"
  show GcLm = "Lm"
  show GcLo = "Lo"
  show GcMn = "Mn"
  show GcMc = "Mc"
  show GcMe = "Me"
  show GcNd = "Nd"
  show GcNl = "Nl"
  show GcNo = "No"
  show GcZs = "Zs"
  show GcZl = "Zl"
  show GcZp = "Zp"
  show GcPc = "Pc"
  show GcPd = "Pd"
  show GcPs = "Ps"
  show GcPe = "Pe"
  show GcPi = "Pi"
  show GcPf = "Pf"
  show GcPo = "Po"
  show GcSm = "Sm"
  show GcSc = "Sc"
  show GcSk = "Sk"
  show GcSo = "So"
  show GcCc = "Cc"
  show GcCf = "Cf"
  show GcCs = "Cs"
  show GcCo = "Co"
  show GcCn = "Cn"

data GCMajorClass
  = ClLetter
  | ClMark
  | ClNumber
  | ClSeparator
  | ClPunctuation
  | ClSymbol
  | ClOther
  deriving (Eq)

instance Show GCMajorClass where
  show ClLetter = "Letter"
  show ClMark = "Mark"
  show ClNumber = "Number"
  show ClSeparator = "Separator"
  show ClPunctuation = "Punctuation"
  show ClSymbol = "Symbol"
  show ClOther = "Other"

gcMajorClass :: GeneralCategory -> GCMajorClass
gcMajorClass GcLu = ClLetter
gcMajorClass GcLl = ClLetter
gcMajorClass GcLt = ClLetter
gcMajorClass GcLm = ClLetter
gcMajorClass GcLo = ClLetter
gcMajorClass GcMn = ClMark
gcMajorClass GcMc = ClMark
gcMajorClass GcMe = ClMark
gcMajorClass GcNd = ClNumber
gcMajorClass GcNl = ClNumber
gcMajorClass GcNo = ClNumber
gcMajorClass GcZs = ClSeparator
gcMajorClass GcZl = ClSeparator
gcMajorClass GcZp = ClSeparator
gcMajorClass GcPc = ClPunctuation
gcMajorClass GcPd = ClPunctuation
gcMajorClass GcPs = ClPunctuation
gcMajorClass GcPe = ClPunctuation
gcMajorClass GcPi = ClPunctuation
gcMajorClass GcPf = ClPunctuation
gcMajorClass GcPo = ClPunctuation
gcMajorClass GcSm = ClSymbol
gcMajorClass GcSc = ClSymbol
gcMajorClass GcSk = ClSymbol
gcMajorClass GcSo = ClSymbol
gcMajorClass GcCc = ClOther
gcMajorClass GcCf = ClOther
gcMajorClass GcCs = ClOther
gcMajorClass GcCo = ClOther
gcMajorClass GcCn = ClOther
