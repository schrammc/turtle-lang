{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Turtle.Frontend.ParsedAST (
    ParsedAST,
    Literal (..),
    Statement (..),
    Ident (..),
    Expression (..),
    TypeIdent (..),
    ASTType (..),
    FunctionParams (..),
    IdentWithType (..),
) where

import Data.Functor.Classes (Eq1, Show1)
import Data.Kind (Type)
import Data.List.NonEmpty
import Data.Text (Text)
import Generics.SOP.TH

newtype Literal = NumLit Double
    deriving (Show, Eq)

deriveGeneric (''Literal)

newtype Ident = Ident Text
    deriving (Show, Eq)

deriveGeneric (''Ident)

newtype TypeIdent = TypeIdent Text
    deriving (Show, Eq)

deriveGeneric (''TypeIdent)

newtype FunctionParams = FunctionParams [ASTType]
    deriving (Show, Eq)
data ASTType
    = ASTType TypeIdent
    | FuncType FunctionParams ASTType
    deriving (Show, Eq)
deriveGeneric (''FunctionParams)
deriveGeneric (''ASTType)

data IdentWithType = IdentWithType Ident ASTType
    deriving (Show, Eq)
deriveGeneric (''IdentWithType)

data Expression (f :: Type -> Type)
    = ELiteral Literal
    | EIdentifier Ident
    | EList [f (Expression f)]

deriveGeneric (''Expression)

deriving instance (Show1 f) => Show (Expression f)
deriving instance (Eq1 f) => Eq (Expression f)

data Statement (f :: Type -> Type)
    = Assignment (f Ident) (f (Expression f))
    | StatementExpression (f (Expression f))
    | If (f (Expression f)) (NonEmpty (f (Statement f))) (NonEmpty (f (Statement f)))
    | FunDecl (f Ident) [f IdentWithType] (f ASTType) (NonEmpty (f (Statement f)))

deriveGeneric (''Statement)

deriving instance (Show1 f) => Show (Statement f)
deriving instance (Eq1 f) => Eq (Statement f)

type ParsedAST (f :: Type -> Type) = [f (Statement f)]