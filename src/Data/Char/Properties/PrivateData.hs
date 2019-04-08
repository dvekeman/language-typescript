module Data.Char.Properties.PrivateData where

import Prelude

handleOmittedRange :: a -> (Char -> a) -> Char -> a
handleOmittedRange def _ c
  | (c < '\x3400') = def -- optimisiation, catch about 99.9% of cases for non-CJK text
handleOmittedRange _ f c
  | (c > '\x3400') && (c <= '\x4DB5') = f '\x3400'
handleOmittedRange _ f c
  | (c > '\x4E00') && (c <= '\x9FA5') = f '\x4E00'
handleOmittedRange _ f c
  | (c > '\xAC00') && (c <= '\xD7A3') = f '\xAC00'
handleOmittedRange _ f c
  | (c > '\xD800') && (c <= '\xDB7F') = f '\xD800'
handleOmittedRange _ f c
  | (c > '\xDB80') && (c <= '\xDBFF') = f '\xDB80'
handleOmittedRange _ f c
  | (c > '\xDC00') && (c <= '\xDFFF') = f '\xDC00'
handleOmittedRange _ f c
  | (c > '\xE000') && (c <= '\xF8FF') = f '\xE000'
handleOmittedRange _ f c
  | (c > '\x20000') && (c <= '\x2A6D6') = f '\x20000'
handleOmittedRange _ f c
  | (c > '\xF0000') && (c <= '\xFFFFD') = f '\xF0000'
handleOmittedRange _ f c
  | (c > '\x100000') && (c <= '\x10FFFD') = f '\x100000'
handleOmittedRange def _ _ = def
