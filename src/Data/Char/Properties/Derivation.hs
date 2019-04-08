module Data.Char.Properties.Derivation where

import Data.Char.Properties.GeneralCategory
import Data.Char.Properties.MiscData
        -- | Combining Grapheme Joiner character.

isCGJ :: Char -> Bool
isCGJ '\x034F' = True
isCGJ _ = False
        -- | Characters with the Math property. For more information, see Chapter 4, Character Properties.
        --
        -- Math = Sm + Other_Math.

isMath :: Char -> Bool
isMath c =
  case getGeneralCategory c
                -- Categories
        of
    GcSm -> True
                -- Properties
    _
      | isOtherMath c -> True
                -- Unspecified
    _ -> False
        -- | Characters with the Alphabetic property. For more information, see Chapter 4, Character Properties.
        --
        -- Alphabetic = Lu+Ll+Lt+Lm+Lo+ Other_Alphabetic.

isAlphabetic :: Char -> Bool
isAlphabetic c =
  case getGeneralCategory c
                -- Categories
        of
    GcLu -> True
    GcLl -> True
    GcLt -> True
    GcLm -> True
    GcLo -> True
                -- Properties
    _
      | isOtherAlphabetic c -> True
                -- Unspecified
    _ -> False
        -- | Characters with the Lowercase property. For more information, see Chapter 4, Character Properties and UAX #21: Case Mappings.
        --
        -- Lowercase = Ll + Other_Lowercase.

isLowercase :: Char -> Bool
isLowercase c =
  case getGeneralCategory c
                -- Categories
        of
    GcLl -> True
                -- Properties
    _
      | isOtherLowercase c -> True
                -- Unspecified
    _ -> False
        -- | Characters with the Uppercase property. For more information, see Chapter 4, Character Properties and UAX #21: Case Mappings.
        --
        -- Uppercase = Lu + Other_Uppercase.

isUppercase :: Char -> Bool
isUppercase c =
  case getGeneralCategory c
                -- Categories
        of
    GcLu -> True
                -- Properties
    _
      | isOtherUppercase c -> True
                -- Unspecified
    _ -> False
        -- | Characters that can start an identifier.
        --
        -- ID_Start = Lu+Ll+Lt+Lm+Lo+Nl.

isIDStart :: Char -> Bool
isIDStart c =
  case getGeneralCategory c
                -- Categories
        of
    GcLu -> True
    GcLl -> True
    GcLt -> True
    GcLm -> True
    GcLo -> True
    GcNl -> True
                -- Properties
                -- Unspecified
    _ -> False
        -- | Characters that can continue an identifier. See Cf Note.
        --
        -- ID_Continue = ID_Start + Mn+Mc+Nd+Pc.

isIDContinue :: Char -> Bool
isIDContinue c =
  case getGeneralCategory c
                -- Categories
        of
    GcMn -> True
    GcMc -> True
    GcNd -> True
    GcPc -> True
                -- Properties
    _
      | isIDStart c -> True
                -- Unspecified
    _ -> False
        -- | For programmatic determination of default-ignorable code points. New characters that should be ignored in processing (unless explicitly supported) will be assigned in these ranges, permitting programs to correctly handle the default behavior of such characters when not otherwise supported. For more information, see UTR #29: Text Boundaries (in proposed draft status at release time for Unicode 3.2).
        --
        -- Default_Ignorable_Code_Point = Other_Default_Ignorable_Code_Point + Cf + Cc + Cs - White_Space.

isDefaultIgnorableCodePoint :: Char -> Bool
isDefaultIgnorableCodePoint c =
  case getGeneralCategory c
                -- Categories
        of
    GcCf -> True
    GcCc -> True
    GcCs -> True
                -- Properties
    _
      | isOtherDefaultIgnorableCodePoint c -> True
    _
      | isWhiteSpace c -> False
                -- Unspecified
    _ -> False
        -- | For programmatic determination of grapheme cluster boundaries. For more information, see UTR #29: Text Boundaries (in proposed draft status at publication of Unicode 3.2).
        --
        -- Grapheme_Base = [0..10FFFF] - Cc - Cf - Cs - Co - Cn - Zl - Zp - Grapheme_Extend - Grapheme_Link - CGJ.

isGraphemeBase :: Char -> Bool
isGraphemeBase c =
  case getGeneralCategory c
                -- Categories
        of
    GcCc -> False
    GcCf -> False
    GcCs -> False
    GcCo -> False
    GcCn -> False
    GcZl -> False
    GcZp -> False
                -- Properties
    _
      | isGraphemeExtend c -> False
    _
      | isGraphemeLink c -> False
    _
      | isCGJ c -> False
                -- Unspecified
    _ -> True
        -- | For programmatic determination of grapheme cluster boundaries. For more information, see UTR #29: Text Boundaries (in proposed draft status at publication of Unicode 3.2).
        --
        -- Grapheme_Extend = Me + Mn + Mc + Other_Grapheme_Extend - Grapheme_Link - CGJ.

isGraphemeExtend :: Char -> Bool
isGraphemeExtend c =
  case getGeneralCategory c
                -- Categories
        of
    GcMe -> True
    GcMn -> True
    GcMc -> True
                -- Properties
    _
      | isOtherGraphemeExtend c -> True
    _
      | isGraphemeLink c -> False
    _
      | isCGJ c -> False
                -- Unspecified
    _ -> False
