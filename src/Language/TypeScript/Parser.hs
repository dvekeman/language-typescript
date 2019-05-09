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
{-# LANGUAGE FlexibleContexts #-}

module Language.TypeScript.Parser (
    declarationSourceFile,
    nextIdentifier
) where

import Language.TypeScript.Types
import Language.TypeScript.Lexer

import Text.Parsec
import Control.Applicative (Applicative(..), (<$>), (<*>), (<*), (*>))
import Data.Char (digitToInt, isHexDigit, isSpace)
import Data.Char.Properties.Misc (isIDStart, isIDContinue)
import Data.Functor.Identity (Identity)
import Data.List (nub)

import Debug.Trace
import Data.Functor (($>))

commentPlaceholder :: ParsecT String u Identity (Either (Line, Column) Comment)
commentPlaceholder = do
  maybeAComment <- optionMaybe multiLineComment
  case maybeAComment of
    Just comment -> return $ Right $ Comment comment []
    Nothing -> fmap toOffset getPosition
  where toOffset pos = Left (sourceLine pos, sourceColumn pos)

nextIdentifier :: ParsecT String u Identity String
nextIdentifier =
    skipMany (choice  (map (try . reserved) [ "export", "declare", "public", "private", "static" ]))
    >> choice (map (try . reserved) [ "var", "function", "class", "interface", "enum", "module" ])
    >> identifier


declarationSourceFile :: ParsecT String u Identity [DeclarationElement]
declarationSourceFile = stripBOM >> whiteSpace >> many declarationElement <* eof


stripBOM :: ParsecT String u Identity ()
stripBOM = optional (char '\65279')


exported :: ParsecT String u Identity Exported
exported = reserved "export" >> return Exported

exportedAs :: ParsecT String u Identity Exported
exportedAs = reserved "export" >> optionMaybe (reserved "as") >> return Exported

defaulted :: ParsecT String u Identity Defaulted
defaulted = reserved "default" >> return Defaulted

constEnum :: ParsecT String u Identity ConstEnum
constEnum = reserved "const" >> return ConstEnum


declarationElement :: ParsecT String u Identity DeclarationElement
declarationElement = choice $ map try
  [ InterfaceDeclaration <$> commentPlaceholder <*> optionMaybe exported <*> interface
  , TypeAliasDeclaration <$> commentPlaceholder <*> optionMaybe exported <*> typeAlias
  , ExportDeclaration <$> (reserved "export" >> lexeme (char '=') *> identifier)
  , AmbientDeclaration <$> commentPlaceholder <*> optionMaybe exported <*> (reserved "declare" *> ambientDeclaration)
  , AmbientDeclaration <$> commentPlaceholder <*> optionMaybe exportedAs <* optionMaybe defaulted <*> ambientDeclaration
  , Unsupported <$> many1 (satisfy (/= '\n')) <* semi
  ]

ambientDeclaration :: ParsecT String u Identity Ambient
ambientDeclaration = choice (map try
  [ ambientVariableDeclaration
  , ambientFunctionDeclaration
  , ambientClassDeclaration
  , ambientInterfaceDeclaration
  , ambientTypeAliasDeclaration
  , ambientEnumDeclaration
  , ambientModuleDeclaration
  , ambientNamespaceDeclaration
  , ambientExternalModuleDeclaration
  , ambientExternalImportDeclaration
  , ambientImportDeclaration
  ])

-- Ignore 'export' keyword, because:
--
-- TSS(12.1.15) Except for ImportDeclarations, AmbientModuleElements always declare exported entities regardless of
-- whether they include the optional export modifier.

ambientVariableDeclaration :: ParsecT String u Identity Ambient
ambientVariableDeclaration = AmbientVariableDeclaration <$> commentPlaceholder <*> (reserved "var" *> identifier) <*> (optionMaybe typeAnnotation <* semi)


ambientFunctionDeclaration :: ParsecT String u Identity Ambient
ambientFunctionDeclaration = AmbientFunctionDeclaration <$> commentPlaceholder <*> (reserved "function" *> identifier) <*> (parameterListAndReturnType <* semi)


ambientClassDeclaration :: ParsecT String u Identity Ambient
ambientClassDeclaration = AmbientClassDeclaration <$> commentPlaceholder <*> (reserved "class" *> identifier) <*> optionMaybe typeParameters <*> optionMaybe extendsClause <*> optionMaybe implementsClause <*> braces (sepEndBy ambientClassBodyElement semi)


ambientInterfaceDeclaration :: ParsecT String u Identity Ambient
ambientInterfaceDeclaration = AmbientInterfaceDeclaration <$> interface


ambientEnumDeclaration :: ParsecT String u Identity Ambient
ambientEnumDeclaration = AmbientEnumDeclaration <$> commentPlaceholder <*> optionMaybe constEnum <*> (reserved "enum" *> identifier) <*> braces (sepEndBy enumMember comma)
  where
  enumMember = (,) <$> propertyName <*> optionMaybe (lexeme (char '=') >> integer)


ambientTypeAliasDeclaration :: ParsecT String u Identity Ambient
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

ambientModuleDeclaration :: ParsecT String u Identity Ambient
ambientModuleDeclaration = AmbientModuleDeclaration <$> commentPlaceholder <*> (reserved "module" *> sepBy identifier dot) <*> braces (many (optionMaybe exported *> ambientDeclaration))


ambientNamespaceDeclaration :: ParsecT String u Identity Ambient
ambientNamespaceDeclaration = AmbientNamespaceDeclaration <$> commentPlaceholder <*> (reserved "namespace" *> sepBy identifier dot) <*>
  choice (map try
    [ braces (many (optionMaybe exported *> ambientDeclaration))
    , semi $> []
    ]
  )

ambientExternalModuleDeclaration :: ParsecT String u Identity Ambient
ambientExternalModuleDeclaration = AmbientExternalModuleDeclaration <$> commentPlaceholder <*> (reserved "module" *> stringLiteral) <*> braces (many ambientExternalModuleElement)


ambientExternalModuleElement :: ParsecT String u Identity AmbientExternalModuleElement
ambientExternalModuleElement = choice (map try
  [ AmbientModuleElement <$> (optionMaybe exported *> ambientDeclaration)
  , exportAssignment ])


ambientImportDeclaration :: ParsecT String u Identity Ambient
ambientImportDeclaration =
--  choice $ map try [
    AmbientImportDeclaration <$> commentPlaceholder
                             <*> (reserved "import" *> identifier)
                             <*> (lexeme (char '=') *> entityName <* semi)
--  -- import { foo , bar } from "module-name/path/to/specific/un-exported/file";
--  , AmbientPathImportDeclaration <$> commentPlaceholder
--                             <*> ( reserved "import" *> between (symbol "{") (symbol "}") (commaSep (optionMaybe (reserved "default as") *> identifier)) )
--                             <*> ( reserved "from" *> between quote quote (optionMaybe (string "./") *> identifier `sepBy` char '/') <* semi )
--  ]


ambientExternalImportDeclaration :: ParsecT String u Identity Ambient
ambientExternalImportDeclaration =
  AmbientExternalImportDeclaration <$> commentPlaceholder
                                   <*> (reserved "import" *> identifier)
                                   <*> (lexeme (char '=') *> reserved "require" *> parens stringLiteral <* semi)


exportAssignment :: ParsecT String u Identity AmbientExternalModuleElement
exportAssignment = ExportAssignment <$> (optionMaybe exported *> lexeme (char '=') *> identifier <* semi)


ambientClassBodyElement :: ParsecT String u Identity (Either (Line, Column) Comment, AmbientClassBodyElement)
ambientClassBodyElement = (,) <$> commentPlaceholder <*> choice (map try
  [ ambientConstructorDeclaration
  , ambientMemberDeclaration
  , ambientIndexSignature ])


ambientConstructorDeclaration :: ParsecT String u Identity AmbientClassBodyElement
ambientConstructorDeclaration = AmbientConstructorDeclaration <$> (reserved "constructor" *> parameterList)


ambientMemberDeclaration :: ParsecT String u Identity AmbientClassBodyElement
ambientMemberDeclaration = AmbientMemberDeclaration <$> optionMaybe publicOrPrivate <*> optionMaybe static <*> propertyName <*> choice [fmap Right parameterListAndReturnType, fmap Left (optionMaybe typeAnnotation)]


ambientIndexSignature :: ParsecT String u Identity AmbientClassBodyElement
ambientIndexSignature = AmbientIndexSignature <$> indexSignature


interface :: ParsecT String u Identity Interface
interface = Interface <$> commentPlaceholder <*> (reserved "interface" *> identifier) <*> optionMaybe typeParameters <*> optionMaybe extendsClause <*> objectType


typeAlias :: ParsecT String u Identity TypeAlias
typeAlias =
  TypeAlias <$> commentPlaceholder <*> typeAliasName <*> (lexeme (char '=') *> _type <* semi)
  -- TODO: this is currently not supported: export type LocationDescriptor<S = LocationState> = Path
  where typeAliasName = (reserved "type" *> identifier) <* optionMaybe typeParameters


extendsClause :: ParsecT String u Identity [TypeRef]
extendsClause = reserved "extends" >> classOrInterfaceTypeList


implementsClause :: ParsecT String u Identity [TypeRef]
implementsClause = reserved "implements" >> classOrInterfaceTypeList


classOrInterfaceTypeList :: ParsecT String u Identity [TypeRef]
classOrInterfaceTypeList = commaSep typeRef


objectType :: ParsecT String u Identity TypeBody
objectType = braces typeBody


typeBody :: ParsecT String u Identity TypeBody
typeBody = TypeBody <$> sepEndBy typeMember semi
  where
  typeMember = (,) <$> commentPlaceholder <*> choice ( map try [ methodSignature, propertySignature, callSignature, constructSignature, typeIndexSignature ])


propertySignature :: ParsecT String u Identity TypeMember
propertySignature = PropertySignature <$> propertyName <*> optionMaybe (lexeme (char '?' >> return Optional)) <*> optionMaybe typeAnnotation


propertyName :: ParsecT String u Identity String
propertyName = identifier <|> stringLiteral <|> numericLiteral


numericLiteral :: ParsecT String u Identity String
numericLiteral =
  (show <$> try decimal) <|>
  (show <$> try binary) <|>
  (show <$> try hexadecimal) <|>
  (show <$> try octal)
  where
    binary = do { _ <- oneOf "bB"; number 2 binDigit }
    binDigit = satisfy (`elem` ['0', '1']) <?> "binary digit"


typeAnnotation :: ParsecT String u Identity Type
typeAnnotation = colon >> _type


callSignature :: ParsecT String u Identity TypeMember
callSignature = CallSignature <$> parameterListAndReturnType


parameterListAndReturnType :: ParsecT String u Identity ParameterListAndReturnType
parameterListAndReturnType = ParameterListAndReturnType <$> optionMaybe typeParameters <*> parameterList <*> optionMaybe typeAnnotation


parameterList :: ParsecT String u Identity [Parameter]
parameterList = parens (commaSep parameter)


parameter :: ParsecT String u Identity Parameter
parameter = choice
  [ try $ RequiredOrOptionalParameter <$> optionMaybe publicOrPrivate <*> identifier <*> optionMaybe (lexeme (char '?' >> return Optional)) <*> optionMaybe parameterAnnotation
  , RestParameter <$> (lexeme (string "...") *> identifier) <*> optionMaybe typeAnnotation
  ]


parameterAnnotation :: ParsecT String u Identity ParameterType
parameterAnnotation = do
  _ <- colon
  choice $ map try
    [ ParameterSpecialized <$> stringLiteral
    , ParameterType <$> _type
    ]


static :: ParsecT String u Identity Static
static = reserved "static" >> return Static


publicOrPrivate :: ParsecT String u Identity PublicOrPrivate
publicOrPrivate = choice
  [ reserved "public" >> return Public
  , reserved "private" >> return Private ]


stringOrNumber :: ParsecT String u Identity StringOrNumber
stringOrNumber = choice
  [ reserved "string" >> return String
  , reserved "number" >> return Number ]


constructSignature :: ParsecT String u Identity TypeMember
constructSignature = ConstructSignature <$> (reserved "new" *> optionMaybe typeParameters) <*> parameterList <*> optionMaybe typeAnnotation


typeIndexSignature :: ParsecT String u Identity TypeMember
typeIndexSignature = TypeIndexSignature <$> indexSignature


indexSignature :: ParsecT String u Identity IndexSignature
indexSignature = squares (IndexSignature <$> identifier <*> (colon *> stringOrNumber)) <*> typeAnnotation


methodSignature :: ParsecT String u Identity TypeMember
methodSignature = MethodSignature <$> propertyName <*> optionMaybe (lexeme (char '?' >> return Optional)) <*> parameterListAndReturnType


typeParameters :: ParsecT String u Identity [TypeParameter]
typeParameters = angles $ commaSep1 typeParameter


typeParameter :: ParsecT String u Identity TypeParameter
typeParameter =
  choice $ map try
  [ PartialTypeParameter <$> (reserved "Partial" *> optionMaybe ( choice ( map try [reserved "extends", reserved "=" ] ) >> _type))
  , TypeParameter <$> identifier <*> optionMaybe ( choice ( map try [reserved "extends", reserved "=" ] ) >> _type)
  ]


fold :: Stream s m t => ParsecT s u m a -> ParsecT s u m b -> (a -> b -> a) -> ParsecT s u m a
fold first more combine = do
  a <- first
  bs <- many more
  return $ foldl combine a bs


_type :: ParsecT String u Identity Type
_type = lexeme $ choice [ try functionType, try constructorType, unionType ]
  where
  unionType = do
    t1 <-
--      trace "arrayType"
      arrayType
    munion <- optionMaybe (lexeme (string "|"))
    case munion of
      Nothing -> return t1
      Just _ -> UnionType t1 <$> unionType
  arrayType = fold atomicType (squares whiteSpace) (flip $ const ArrayType)
  -- unionType = UnionType <$> sepBy1 atomicType (lexeme (string "|"))
  atomicType =
    choice $ map try
      [ parens _type
      , TypeQuery <$> typeQuery
      , Predefined <$> commentPlaceholder <*> predefinedType
      , TypeReference <$> typeRef
      , ObjectType <$> objectType
      , TupleType <$> squares (_type `sepBy` lexeme (string ","))
      ]
  functionType = FunctionType <$> optionMaybe typeParameters <*> parameterList <*> returnType
  constructorType = ConstructorType <$> (reserved "new" *> optionMaybe typeParameters) <*> parameterList <*> returnType
  returnType = lexeme (string "=>") *> _type
  typeQuery = reserved "typeof" *> sepBy1 identifier dot


typeRef :: ParsecT String u Identity TypeRef
typeRef = TypeRef <$> typeName <*> optionMaybe typeArguments


predefinedType :: ParsecT String u Identity PredefinedType
predefinedType = choice
  [ (reserved "any" <|> reserved "object") >> return AnyType
  , (reserved "number" <|> reserved "Integer" <|> reserved "int") >> return (NumberType Nothing)
  , (reserved "boolean" <|> reserved "bool") >> return BooleanType
  , reserved "string" >> return (StringType Nothing)
  , reserved "void" >> return VoidType
  , reserved "null" >> return NullType
  -- TODO: need to rethink this through
  , stringLiteral >>= \val -> return (StringType (Just [val]))
  , lexeme numericLiteral >>= \val -> return (NumberType (Just val))
  ]


modulePath :: ParsecT String u Identity EntityName
modulePath = lexeme (char '\'') *> (EntityName Nothing <$> manyTill anyChar (char '\''))

entityName :: ParsecT String u Identity EntityName
entityName = fmap toEntityName (sepBy1 identifier dot)
  where
  toEntityName [t] = EntityName Nothing t
  toEntityName ts = EntityName (Just $ ModuleName $ init ts) (last ts)


typeName :: ParsecT String u Identity TypeName
typeName = fmap toTypeName (sepBy1 identifier dot)
  where
  toTypeName [t] = TypeName Nothing t
  toTypeName ts = TypeName (Just $ ModuleName $ init ts) (last ts)


typeArguments :: ParsecT String u Identity [Type]
typeArguments = angles $ commaSep1 _type


stringLiteral :: ParsecT String u Identity String
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


identifier :: ParsecT String u Identity String
identifier = lexeme $ try $ do
  name <- ident
  if name `elem` reservedNames
    then unexpected $ "reserved word " ++ show name
    else return name


ident :: ParsecT String u Identity String
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


hexEscapeSequence :: ParsecT String u Identity Char
hexEscapeSequence = do
  c0 <- hexDigit
  c1 <- hexDigit
  toEnum . fromInteger <$> numberBase 16 [c0, c1]


unicodeEscapeSequence :: ParsecT String u Identity Char
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
number :: Stream s m t => Integer -> ParsecT s u m Char -> ParsecT s u m Integer
number base baseDigit = numberBase base =<< many1 baseDigit


numberBase :: (Monad m, Foldable t) => Integer -> t Char -> m Integer
numberBase base digits = do
  let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
  seq n (return n)


-- Copied (and altered) from Text.Parsec.Token
-- Note: only multiline comments
multiLineComment :: ParsecT String u Identity [String]
multiLineComment =
    do { _ <- try (do{ _ <- string "/*" ; skipMany1 (satisfy (\c -> c == ' ' || c == '*'));})
       ; inComment
       }


inComment :: ParsecT String u Identity [String]
inComment = do
  comments <- inCommentMulti []
  return $ reverse comments

{-
-}
inCommentMulti :: [String] -> ParsecT String u Identity [String]
inCommentMulti comments
    =   -- Look for the end of the multiline comment preceded by spaces
        -- Also cater for 'text on the last line*/'
        do{
            lastcomment <-
              try (do { skipMany (satisfy isSpace);
                        manyTill notEndOfLine (try (string "*/"))
                      }
                  );
            skipMany1 (satisfy isSpace);
            return $
              if trim lastcomment == ""
              then comments
              else lastcomment:comments
          }
        -- Parses an actual comment line. Skip the trailing spaces followed by a '*'
    <|> do{ _ <- try (do { skipMany (satisfy isSpace); string "*" });
            newcomment <- manyTill anyChar endOfLine ;
            inCommentMulti (newcomment:comments)
          }
    <?> "end of comment"



notEndOfLine :: ParsecT String u Identity Char
notEndOfLine     = try (do{ c <- try endOfLine; unexpected (show c) }
                           <|> anyChar
                       )

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

quote :: ParsecT String u Identity String
quote = choice $ map try [ symbol "'", symbol "\"" ]