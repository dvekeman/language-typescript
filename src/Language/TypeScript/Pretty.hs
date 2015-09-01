-----------------------------------------------------------------------------
--
-- Module      :  Language.TypeScript.Pretty
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

module Language.TypeScript.Pretty (
  renderDeclarationSourceFile
) where

import Language.TypeScript.Types
import Text.PrettyPrint

renderDeclarationSourceFile :: [DeclarationElement] -> String
renderDeclarationSourceFile = render . declarationSourceFile

declarationSourceFile :: [DeclarationElement] -> Doc
declarationSourceFile = vcat . map declarationElement

exported :: Exported -> Doc
exported _ = text "export"

renderMaybe :: (a -> Doc) -> Maybe a -> Doc
renderMaybe f = maybe empty f

stringLiteral :: String -> Doc
stringLiteral = doubleQuotes . text

declarationElement :: DeclarationElement -> Doc
declarationElement (InterfaceDeclaration _ e i) =
  renderMaybe exported e
  <+> interface i
declarationElement (ExportDeclaration name) =
  exported Exported
  <+> text "="
  <+> text name
declarationElement (AmbientDeclaration _ e a) =
  renderMaybe exported e
  <+> text "declare"
  <+> renderAmbientDeclaration a
declarationElement (TypeAliasDeclaration _ e a) =
  renderMaybe exported e
  <+> renderTypeAlias a

renderAmbientDeclaration :: Ambient -> Doc
renderAmbientDeclaration (AmbientVariableDeclaration _ name ty) =
  text "var"
  <+> text name
  <+> renderMaybe typeAnnotation ty
  <+> semi
renderAmbientDeclaration (AmbientFunctionDeclaration _ name plrt) =
  text "function"
  <+> text name
  <+> parameterListAndReturnType plrt
  <+> semi
renderAmbientDeclaration (AmbientClassDeclaration _ name ps exts imps els) =
  text "class"
  <+> text name
  <+> renderMaybe typeParameters ps
  <+> renderMaybe extendsClause exts
  <+> renderMaybe implementsClause imps
  <+> braces (sepEndBy semi (renderAmbientClassBodyElement . snd ) els)
renderAmbientDeclaration (AmbientInterfaceDeclaration i) = interface i
renderAmbientDeclaration (AmbientEnumDeclaration _ mconst name members) =
  renderMaybe (\ConstEnum -> text "const") mconst <+>
  text "enum" <+> text name <+> braces (sepEndBy comma enumMember members)
  where
  enumMember (name, val) = text name <+> renderMaybe (\n -> char '=' <+> integer n) val
renderAmbientDeclaration (AmbientModuleDeclaration _ name ds) =
  text "module"
  <+> sepBy dot text name
  <+> braces (vcat (map renderAmbientDeclaration ds))
renderAmbientDeclaration (AmbientExternalModuleDeclaration _ name es) =
  text "module"
  <+> stringLiteral name
  <+> braces (vcat (map renderAmbientExternalModuleElement es))
renderAmbientDeclaration (AmbientTypeAliasDeclaration a) =
  renderTypeAlias a
renderAmbientDeclaration (AmbientImportDeclaration _ name entityName) =
  text "import"
  <+> text name
  <+> char '='
  <+> renderEntityName entityName
renderAmbientDeclaration (AmbientExternalImportDeclaration _ name imp) =
  text "import"
  <+> text name
  <+> char '='
  <+> text "require"
  <+> stringLiteral imp

renderAmbientExternalModuleElement :: AmbientExternalModuleElement -> Doc
renderAmbientExternalModuleElement (AmbientModuleElement a) = renderAmbientDeclaration a
renderAmbientExternalModuleElement (ExportAssignment name) =
  text "export"
  <+> char '='
  <+> text name
  <+> semi

renderAmbientClassBodyElement :: AmbientClassBodyElement -> Doc
renderAmbientClassBodyElement (AmbientConstructorDeclaration ps) =
  text "constructor"
  <+> parameterList ps
  <+> semi
renderAmbientClassBodyElement (AmbientMemberDeclaration p s prop (Left ty)) =
  renderMaybe publicOrPrivate p
  <+> renderMaybe static s
  <+> propertyName prop
  <+> renderMaybe typeAnnotation ty
renderAmbientClassBodyElement (AmbientMemberDeclaration p s prop (Right ps)) =
  renderMaybe publicOrPrivate p
  <+> renderMaybe static s
  <+> propertyName prop
  <+> parameterListAndReturnType ps
renderAmbientClassBodyElement (AmbientIndexSignature i) = renderIndexSignature i

renderIndexSignature :: IndexSignature -> Doc
renderIndexSignature (IndexSignature s sn ty) =
  text s
  <+> colon
  <+> stringOrNumber sn
  <+> typeAnnotation ty

renderTypeAlias :: TypeAlias -> Doc
renderTypeAlias (TypeAlias _ name ty) =
  text "type"
  <+> text name
  <+> text "="
  <+> _type ty

dot :: Doc
dot = char '.'

sepEndBy :: Doc -> (a -> Doc) -> [a] -> Doc
sepEndBy s f as = hsep $ map (\e -> f e <+> s) as

renderEntityName :: EntityName -> Doc
renderEntityName (EntityName Nothing e) = text e
renderEntityName (EntityName (Just (ModuleName es)) e) = hcat (punctuate dot (map text es)) <> text e

interface :: Interface -> Doc
interface (Interface _ name ps exts ty) =
  text "interface"
  <+> text name
  <+> renderMaybe typeParameters ps
  <+> renderMaybe extendsClause exts
  <+> objectType ty

extendsClause :: [TypeRef] -> Doc
extendsClause rs = text "extends" <+> classOrInterfaceTypeList rs

implementsClause :: [TypeRef] -> Doc
implementsClause rs = text "implements" <+> classOrInterfaceTypeList rs

sepBy :: Doc -> (a -> Doc) -> [a] -> Doc
sepBy s f as = hsep $ punctuate s (map f as)

commaSep :: (a -> Doc) -> [a] -> Doc
commaSep = sepBy comma

classOrInterfaceTypeList :: [TypeRef] -> Doc
classOrInterfaceTypeList = commaSep typeRef

objectType :: TypeBody -> Doc
objectType = braces . typeBody

typeBody :: TypeBody -> Doc
typeBody (TypeBody ms) = hcat . map (\(_, m) -> typeMember m <+> semi) $ ms

typeMember :: TypeMember -> Doc
typeMember (MethodSignature name opt plrt) =
  propertyName name
  <+> renderMaybe optional opt
  <+> parameterListAndReturnType plrt
typeMember (PropertySignature name opt ty) =
  propertyName name
  <+> renderMaybe optional opt
  <+> renderMaybe typeAnnotation ty
typeMember (CallSignature plrt) = parameterListAndReturnType plrt
typeMember (ConstructSignature tyArgs pl ty) =
  text "new"
  <+> renderMaybe typeParameters tyArgs
  <+> parens (parameterList pl)
  <+> renderMaybe typeAnnotation ty
typeMember (TypeIndexSignature i) = renderIndexSignature i

propertyName :: String -> Doc
propertyName = text

typeAnnotation :: Type -> Doc
typeAnnotation t = colon <+> _type t

parameterListAndReturnType :: ParameterListAndReturnType -> Doc
parameterListAndReturnType (ParameterListAndReturnType ps pl ty) =
  renderMaybe typeParameters  ps
  <+> parens (parameterList pl)
  <+> renderMaybe typeAnnotation ty

parameterList :: [Parameter] -> Doc
parameterList = commaSep parameter

optional :: Optional -> Doc
optional _ = char '?'

parameter :: Parameter -> Doc
parameter (RequiredOrOptionalParameter pop name opt ty) =
  renderMaybe publicOrPrivate pop
  <+> text name
  <+> renderMaybe optional opt
  <+> renderMaybe typeAnnotation ty
parameter (RestParameter name ty) =
  text "..."
  <+> text name
  <+> renderMaybe typeAnnotation ty

static :: Static -> Doc
static _ = text "static"

publicOrPrivate :: PublicOrPrivate -> Doc
publicOrPrivate Public = text "public"
publicOrPrivate Private = text "private"

stringOrNumber :: StringOrNumber -> Doc
stringOrNumber String = text "string"
stringOrNumber Number = text "number"

typeParameters :: [TypeParameter] -> Doc
typeParameters ps = char '<' <+> commaSep typeParameter ps <+> char '>'

typeParameter :: TypeParameter -> Doc
typeParameter (TypeParameter name ext) =
  text name
  <+> renderMaybe (\t -> text "extends" <+> _type t) ext

_type :: Type -> Doc
_type (ArrayType t) = _type t <+> text "[]"
_type (Predefined p) = predefinedType p
_type (TypeReference r) = typeRef r
_type (ObjectType o) = objectType o
-- TSS(3.7): Parentheses are required around union, function, or
-- constructor types when they are used as array element types, and
-- parentheses are required around function or constructor types in
-- union types.
--
-- NOTE: instead of implementing this logic, just putting parens
-- around everything.
_type (FunctionType ps pl ret) = parens $
  renderMaybe typeParameters ps
  <+> parens (parameterList pl)
  <+> text "=>"
  <+> _type ret
_type (ConstructorType ps pl ret) = parens $
  text "new"
  <+> renderMaybe typeParameters ps
  <+> parens (parameterList pl)
  <+> text "=>"
  <+> _type ret
_type (UnionType t1 t2) = parens $
  _type t1
  <+> text "|"
  <+> _type t2
_type (TupleType ts) = brackets $
  commaSep _type ts

typeRef :: TypeRef -> Doc
typeRef (TypeRef n as) =
  typeName n
  <+> renderMaybe typeArguments as

predefinedType :: PredefinedType -> Doc
predefinedType AnyType = text "any"
predefinedType NumberType = text "number"
predefinedType BooleanType = text "boolean"
predefinedType StringType = text "string"
predefinedType VoidType = text "void"

typeName :: TypeName -> Doc
typeName (TypeName Nothing t) = text t
typeName (TypeName (Just (ModuleName ts)) t) = sepBy dot text ts <+> text t

typeArguments :: [Type] -> Doc
typeArguments ts = char '<' <+> commaSep _type ts <+> char '>'
