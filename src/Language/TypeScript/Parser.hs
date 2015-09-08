-----------------------------------------------------------------------------
--
-- Module      :  Language.TypeScript.Parser
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

module Language.TypeScript.Parser (
    declarationSourceFile,
    nextIdentifier
) where

import Language.TypeScript.Types
import Language.TypeScript.Lexer

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String (parseFromFile)
import Control.Applicative
       (Applicative(..), (<$>), (<*>), (<*), (*>))
import Data.Char (digitToInt, isHexDigit)
import Data.Char.Properties.Misc (isIDStart, isIDContinue)

commentPlaceholder = fmap toOffset getPosition where
  toOffset pos = Left $ (sourceLine pos, sourceColumn pos)

nextIdentifier =
    skipMany (choice  (map (try . reserved) [ "export", "declare", "public", "private", "static" ]))
    >> choice (map (try . reserved) [ "var", "function", "class", "interface", "enum", "module" ])
    >> identifier

declarationSourceFile = stripBOM >> whiteSpace >> many declarationElement <* eof

stripBOM = optional (char '\65279')

exported = reserved "export" >> return Exported

constEnum = reserved "const" >> return ConstEnum

declarationElement = choice $ map try
  [ InterfaceDeclaration <$> commentPlaceholder <*> optionMaybe exported <*> interface
  , TypeAliasDeclaration <$> commentPlaceholder <*> optionMaybe exported <*> typeAlias
  , ExportDeclaration <$> (reserved "export" >> lexeme (char '=') *> identifier)
  , AmbientDeclaration <$> commentPlaceholder <*> optionMaybe exported <*> (reserved "declare" *> ambientDeclaration)
  ]

ambientDeclaration = choice (map try
  [ ambientVariableDeclaration
  , ambientFunctionDeclaration
  , ambientClassDeclaration
  , ambientInterfaceDeclaration
  , ambientTypeAliasDeclaration
  , ambientEnumDeclaration
  , ambientModuleDeclaration
  , ambientExternalModuleDeclaration
  , ambientExternalImportDeclaration
  , ambientImportDeclaration
  ])

-- Ignore 'export' keyword, because:
--
-- TSS(12.1.15) Except for ImportDeclarations, AmbientModuleElements always declare exported entities regardless of
-- whether they include the optional export modifier.

ambientVariableDeclaration = AmbientVariableDeclaration <$> commentPlaceholder <*> (reserved "var" *> identifier) <*> (optionMaybe typeAnnotation <* semi)

ambientFunctionDeclaration = AmbientFunctionDeclaration <$> commentPlaceholder <*> (reserved "function" *> identifier) <*> (parameterListAndReturnType <* semi)

ambientClassDeclaration = AmbientClassDeclaration <$> commentPlaceholder <*> (reserved "class" *> identifier) <*> optionMaybe typeParameters <*> optionMaybe extendsClause <*> optionMaybe implementsClause <*> braces (sepEndBy ambientClassBodyElement semi)

ambientInterfaceDeclaration = AmbientInterfaceDeclaration <$> interface

ambientEnumDeclaration = AmbientEnumDeclaration <$> commentPlaceholder <*> optionMaybe constEnum <*> (reserved "enum" *> identifier) <*> braces (sepEndBy enumMember comma)
  where
  enumMember = (,) <$> propertyName <*> optionMaybe (lexeme (char '=') >> integer)

ambientTypeAliasDeclaration = AmbientTypeAliasDeclaration <$> typeAlias

-- "optional" keyword is ignored below:
--
-- TSS(12.1.5) Except for ImportDeclarations, AmbientModuleElements
-- always declare exported entities regardless of whether they include
-- the optional export modifier.
--
-- TSS(12.2) If an ambient external module declaration contains no
-- export assignment, entities dñeclared in the module are exported
-- regardless of whether their declarations include the optional
-- export modifier.

ambientModuleDeclaration = AmbientModuleDeclaration <$> commentPlaceholder <*> (reserved "module" *> sepBy identifier dot) <*> braces (many (optionMaybe exported *> ambientDeclaration))

ambientExternalModuleDeclaration = AmbientExternalModuleDeclaration <$> commentPlaceholder <*> (reserved "module" *> stringLiteral) <*> braces (many ambientExternalModuleElement)

ambientExternalModuleElement = choice (map try
  [ AmbientModuleElement <$> (optionMaybe exported *> ambientDeclaration)
  , exportAssignment ])

ambientImportDeclaration =
  AmbientImportDeclaration <$> commentPlaceholder
                           <*> (reserved "import" *> identifier)
                           <*> (lexeme (char '=') *> entityName <* semi)

ambientExternalImportDeclaration =
  AmbientExternalImportDeclaration <$> commentPlaceholder
                                   <*> (reserved "import" *> identifier)
                                   <*> (lexeme (char '=') *> reserved "require" *> parens stringLiteral <* semi)

exportAssignment = ExportAssignment <$> (optionMaybe exported *> lexeme (char '=') *> identifier <* semi)

ambientClassBodyElement = (,) <$> commentPlaceholder <*> (choice $ map try
  [ ambientConstructorDeclaration
  , ambientMemberDeclaration
  , ambientIndexSignature ])

ambientConstructorDeclaration = AmbientConstructorDeclaration <$> (reserved "constructor" *> parameterList)

ambientMemberDeclaration = AmbientMemberDeclaration <$> optionMaybe publicOrPrivate <*> optionMaybe static <*> propertyName <*> choice [fmap Right parameterListAndReturnType, fmap Left (optionMaybe typeAnnotation)]

ambientIndexSignature = AmbientIndexSignature <$> indexSignature

interface = Interface <$> commentPlaceholder <*> (reserved "interface" *> identifier) <*> optionMaybe typeParameters <*> optionMaybe extendsClause <*> objectType

typeAlias = TypeAlias <$> commentPlaceholder <*> (reserved "type" *> identifier) <*> (lexeme (char '=') *> _type <* semi)

extendsClause = reserved "extends" >> classOrInterfaceTypeList

implementsClause = reserved "implements" >> classOrInterfaceTypeList

classOrInterfaceTypeList = commaSep typeRef

objectType = braces typeBody

typeBody = TypeBody <$> sepEndBy typeMember semi
  where
  typeMember = (,) <$> commentPlaceholder <*> (choice $ map try [ methodSignature, propertySignature, callSignature, constructSignature, typeIndexSignature ])

propertySignature = PropertySignature <$> propertyName <*> optionMaybe (lexeme (char '?' >> return Optional)) <*> optionMaybe typeAnnotation

propertyName = identifier <|> stringLiteral <|> numericLiteral

numericLiteral =
  (show <$> try decimal) <|>
  (show <$> try binary) <|>
  (show <$> try hexadecimal) <|>
  (show <$> try octal)
  where
    binary = do { oneOf "bB"; number 2 binDigit }
    binDigit = satisfy (`elem` ['0', '1']) <?> "binary digit"

typeAnnotation = colon >> _type

callSignature = CallSignature <$> parameterListAndReturnType

parameterListAndReturnType = ParameterListAndReturnType <$> optionMaybe typeParameters <*> parameterList <*> optionMaybe typeAnnotation

parameterList = parens (commaSep parameter)

parameter = choice
  [ try $ RequiredOrOptionalParameter <$> optionMaybe publicOrPrivate <*> identifier <*> optionMaybe (lexeme (char '?' >> return Optional)) <*> optionMaybe parameterAnnotation
  , RestParameter <$> (lexeme (string "...") *> identifier) <*> optionMaybe typeAnnotation
  ]

parameterAnnotation = do
  colon
  choice $ map try
    [ ParameterSpecialized <$> stringLiteral
    , ParameterType <$> _type
    ]

static = reserved "static" >> return Static

publicOrPrivate = choice
  [ reserved "public" >> return Public
  , reserved "private" >> return Private ]

stringOrNumber = choice
  [ reserved "string" >> return String
  , reserved "number" >> return Number ]

constructSignature = ConstructSignature <$> (reserved "new" *> optionMaybe typeParameters) <*> parameterList <*> optionMaybe typeAnnotation

typeIndexSignature = TypeIndexSignature <$> indexSignature

indexSignature = squares (IndexSignature <$> identifier <*> (colon *> stringOrNumber)) <*> typeAnnotation

methodSignature = MethodSignature <$> propertyName <*> optionMaybe (lexeme (char '?' >> return Optional)) <*> parameterListAndReturnType

typeParameters = angles $ commaSep1 typeParameter

typeParameter = TypeParameter <$> identifier <*> optionMaybe (reserved "extends" >> _type)
fold :: Stream s m t => ParsecT s u m a -> ParsecT s u m b -> (a -> b -> a) -> ParsecT s u m a
fold first more combine = do
  a <- first
  bs <- many more
  return $ foldl combine a bs

_type = lexeme $ choice [ try functionType, try constructorType, unionType ]
  where
  unionType = do
    t1 <- arrayType
    munion <- optionMaybe (lexeme (string "|"))
    case munion of
      Nothing -> return t1
      Just _ -> UnionType t1 <$> unionType
  arrayType = fold atomicType (squares whiteSpace) (flip $ const ArrayType)
  -- unionType = UnionType <$> sepBy1 atomicType (lexeme (string "|"))
  atomicType = choice $ map try
    [ parens _type
    , TypeQuery <$> typeQuery
    , Predefined <$> predefinedType
    , TypeReference <$> typeRef
    , ObjectType <$> objectType
    , TupleType <$> squares (_type `sepBy` lexeme (string ","))
    ]
  functionType = FunctionType <$> optionMaybe typeParameters <*> parameterList <*> returnType
  constructorType = ConstructorType <$> (reserved "new" *> optionMaybe typeParameters) <*> parameterList <*> returnType
  returnType = lexeme (string "=>") *> _type
  typeQuery = reserved "typeof" *> sepBy1 identifier dot

typeRef = TypeRef <$> typeName <*> optionMaybe typeArguments

predefinedType = choice
  [ reserved "any" >> return AnyType
  , reserved "number" >> return NumberType
  , (reserved "boolean" <|> reserved "bool") >> return BooleanType
  , reserved "string" >> return StringType
  , reserved "void" >> return VoidType
  ]

entityName = fmap toEntityName (sepBy1 identifier dot)
  where
  toEntityName [t] = EntityName Nothing t
  toEntityName ts = EntityName (Just $ ModuleName $ init ts) (last ts)

typeName = fmap toTypeName (sepBy1 identifier dot)
  where
  toTypeName [t] = TypeName Nothing t
  toTypeName ts = TypeName (Just $ ModuleName $ init ts) (last ts)

typeArguments = angles $ commaSep1 _type

stringLiteral = lexeme $ do
    c0 <- oneOf "'\""
    case c0 of
      '\'' -> go True
      '"' -> go False
      _ -> error "impossible case in stringLiteral"
  where
    --FIXME: handle line continuations / terminators? meh
    go isSingle = do
      ch0 <- anyChar
      case (ch0, isSingle) of
        ('\'', True) -> return []
        ('"', False) -> return []
        ('\'', False) -> fail "Unexpected single quote in double quoted string"
        ('"', True) -> fail "Unexpected double quote in single quoted string"
        ('\\', _) -> do
          ch <- anyChar
          r <- case ch of
            '\'' -> return '\''
            '"' -> return '"'
            '\\' -> return '\\'
            'b' -> return '\b'
            'f' -> return '\f'
            'n' -> return '\n'
            'r' -> return '\r'
            't' -> return '\t'
            'v' -> return '\v'
            '0' -> return '\NUL'
            'x' -> hexEscapeSequence
            'u' -> unicodeEscapeSequence
            _ -> return ch
          (r :) <$> go isSingle
        _ -> (ch0 :) <$> go isSingle

identifier = lexeme $ try $ do
  name <- ident
  if name `elem` reservedNames
    then unexpected $ "reserved word " ++ show name
    else return name

ident = do
    c0 <- consumeChar isIDStart
    cs <- many (consumeChar (\c -> isIDContinue c || isZeroWidth c))
    return (c0 : cs)
  where
    isZeroWidth '\8204' = True
    isZeroWidth '\8205' = True
    isZeroWidth _ = False
    consumeChar f = do
      c0 <- satisfy (\c -> c `elem` "$_\\" || f c)
      case c0 of
        '\\' -> char 'u' >> unicodeEscapeSequence
        _ -> return c0

hexEscapeSequence = do
  c0 <- hexDigit
  c1 <- hexDigit
  toEnum . fromInteger <$> numberBase 16 [c0, c1]

unicodeEscapeSequence = do
  c <- satisfy (\c -> c == '{' || isHexDigit c)
  digits <- case c of
    '{' -> manyTill hexDigit (char '}')
    _ -> do
      c0 <- hexDigit
      c1 <- hexDigit
      c2 <- hexDigit
      c3 <- hexDigit
      return [c0, c1, c2, c3]
  toEnum . fromInteger <$> numberBase 16 digits

-- Copied from parsec
number base baseDigit = numberBase base =<< many1 baseDigit
numberBase base digits = do
  let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
  seq n (return n)
