-----------------------------------------------------------------------------
--
-- Module      :  Language.TypeScript.Types
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

{-# LANGUAGE DeriveDataTypeable #-}

module Language.TypeScript.Types where

import qualified Data.Map as M
import Data.Semigroup
import Data.Data (Typeable, Data)

data Comment = Comment
  { commentText     :: [String]
  , commentOther    :: [(String, String)]
  } deriving (Show, Data, Typeable)

instance Monoid Comment where
  mempty = Comment [] []
  mappend (Comment ts os) (Comment ts' os') = Comment (ts ++ ts') (os ++ os')

instance Semigroup Comment where
  (<>) = mappend

type CommentPlaceholder = Either (Int, Int) Comment

data DeclarationElement
  = InterfaceDeclaration CommentPlaceholder (Maybe Exported) Interface
  | TypeAliasDeclaration CommentPlaceholder (Maybe Exported) TypeAlias
  | ExportDeclaration String
  | AmbientDeclaration CommentPlaceholder (Maybe Exported) Ambient
  | Unsupported String
  deriving (Show, Data, Typeable)

data Exported = Exported deriving (Show, Data, Typeable)

data Defaulted = Defaulted deriving (Show, Data, Typeable)

data EntityName = EntityName (Maybe ModuleName) String deriving (Show, Data, Typeable)

data Interface = Interface CommentPlaceholder String (Maybe [TypeParameter]) (Maybe [TypeRef]) TypeBody deriving (Show, Data, Typeable)

data TypeAlias = TypeAlias CommentPlaceholder String Type deriving (Show, Data, Typeable)

data Ambient
  = AmbientVariableDeclaration CommentPlaceholder String (Maybe Type)
  | AmbientFunctionDeclaration CommentPlaceholder String ParameterListAndReturnType
  | AmbientClassDeclaration CommentPlaceholder String (Maybe [TypeParameter]) (Maybe [TypeRef]) (Maybe [TypeRef]) [(CommentPlaceholder, AmbientClassBodyElement)]
  | AmbientInterfaceDeclaration Interface
  | AmbientEnumDeclaration CommentPlaceholder (Maybe ConstEnum) String [(String, Maybe Integer)]
  | AmbientTypeAliasDeclaration TypeAlias
  | AmbientModuleDeclaration CommentPlaceholder [String] [Ambient]
  | AmbientNamespaceDeclaration CommentPlaceholder [String] [Ambient]
  | AmbientExternalModuleDeclaration CommentPlaceholder String [AmbientExternalModuleElement]
  | AmbientImportDeclaration CommentPlaceholder String EntityName
  | AmbientExternalImportDeclaration CommentPlaceholder String String
  deriving (Show, Data, Typeable)

data AmbientExternalModuleElement
  = AmbientModuleElement Ambient
  | ExportAssignment String
  deriving (Show, Data, Typeable)

data TypeRef = TypeRef TypeName (Maybe [Type]) deriving (Show, Data, Typeable)

data AmbientClassBodyElement
  = AmbientConstructorDeclaration [Parameter]
  | AmbientMemberDeclaration (Maybe PublicOrPrivate) (Maybe Static) String (Either (Maybe Type) ParameterListAndReturnType)
  | AmbientIndexSignature IndexSignature
  deriving (Show, Data, Typeable)

data Static = Static deriving (Show, Data, Typeable)

data Optional = Optional deriving (Show, Data, Typeable)

data ConstEnum = ConstEnum deriving (Show, Data, Typeable)

data TypeBody = TypeBody [(CommentPlaceholder, TypeMember)] deriving (Show, Data, Typeable)

data TypeMember
  = PropertySignature String (Maybe Optional) (Maybe Type)
  | CallSignature ParameterListAndReturnType
  | ConstructSignature (Maybe [TypeParameter]) [Parameter] (Maybe Type)
  | TypeIndexSignature IndexSignature
  | MethodSignature String (Maybe Optional) ParameterListAndReturnType
  deriving (Show, Data, Typeable)

data IndexSignature = IndexSignature String StringOrNumber Type deriving (Show, Data, Typeable)

data ParameterListAndReturnType = ParameterListAndReturnType (Maybe [TypeParameter]) [Parameter] (Maybe Type) deriving (Show, Data, Typeable)

data Parameter
  = RequiredOrOptionalParameter (Maybe PublicOrPrivate) String (Maybe Optional) (Maybe ParameterType)
  | RestParameter String (Maybe Type)
  deriving (Show, Data, Typeable)

data ParameterType
  = ParameterType Type
  | ParameterSpecialized String
  deriving (Show, Data, Typeable)

data StringOrNumber = String | Number deriving (Show, Data, Typeable)

data PublicOrPrivate = Public | Private deriving (Show, Data, Typeable)

data TypeParameter =
  PartialTypeParameter (Maybe Type)
  | TypeParameter String (Maybe Type) deriving (Show, Data, Typeable)

data Type
  = Predefined CommentPlaceholder PredefinedType
  | TypeReference TypeRef
  | ObjectType TypeBody
  | ArrayType Type
  | UnionType Type Type
  | TupleType [Type]
  | TypeQuery [String]
  | FunctionType (Maybe [TypeParameter]) [Parameter] Type
  | ConstructorType (Maybe [TypeParameter]) [Parameter] Type
  deriving (Show, Data, Typeable)

data TypeName = TypeName (Maybe ModuleName) String deriving (Show, Data, Typeable)

data ModuleName = ModuleName [String] deriving (Show, Data, Typeable)

data PredefinedType
  = AnyType
  | NumberType (Maybe String)
  | BooleanType
  | StringType (Maybe [String])
  | NullType
  | VoidType
  deriving (Show, Data, Typeable)
