-----------------------------------------------------------------------------
--
-- Module      :  Language.TypeScript.Lexer
-- Copyright   :  (c) DICOM Grid Inc. 2013
-- License     :  MIT
--
-- Maintainer  :  Phillip Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Language.TypeScript.Lexer (
    reserved
  , integer
  , decimal
  , hexadecimal
  , octal
  , lexeme
  , whiteSpace
  , parens
  , braces
  , angles
  , brackets
  , squares
  , semi
  , comma
  , colon
  , dot
  , commaSep
  , commaSep1
) where

import Text.Parsec
import qualified Text.Parsec.Token as T
import Text.Parsec.Language

typeScriptDef =	javaStyle
  { T.identStart = oneOf "_$" <|> letter
  , T.reservedNames = [
      "break", "do", "instanceof", "typeof", "case", "else", "new", "var", "catch", "finally", "return", "void", "continue", "for",
      "switch", "while", "debugger", "function", "this", "with", "default", "if", "throw", "delete", "in", "try", "class", "enum",
      "extends", "super", "const", "export", "import", "implements", "let", "private", "public", "yield", "interface", "package",
      "protected", "static"
    ]
  , T.caseSensitive = True
  , T.nestedComments = False
  }

parser                = T.makeTokenParser typeScriptDef

reserved              = T.reserved parser
integer               = T.integer parser
decimal               = T.decimal parser
hexadecimal           = T.hexadecimal parser
octal                 = T.octal parser
lexeme                = T.lexeme parser
whiteSpace            = T.whiteSpace parser
parens                = T.parens parser
braces                = T.braces parser
angles                = T.angles parser
brackets              = T.brackets parser
squares               = T.brackets parser
comma                 = T.comma parser
colon                 = T.colon parser
dot                   = T.dot parser
commaSep              = T.commaSep parser
commaSep1             = T.commaSep1 parser

-- Despite the spec not mentioning this, TypeScript has implicit
-- semicolons, and they can be repeated.  So this just consumes them
-- and any whitespace.
semi                  = lexeme (many (lexeme (char ';')))
