{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    Pattern (..),
    Annotated (..),
    astMapM,
    rangesA,
) where

import Data.Data (Data, Typeable, gmapM)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (Semigroup (sconcat))
import Data.Text (Text)
import Generics.SOP.TH (deriveGeneric)
import Language.Turtle.Frontend.Range (Range (..))

data Annotated annotation a = Annotated {annotation :: annotation, annotatedValue :: a}
    deriving (Eq, Show, Typeable, Data)

instance Functor (Annotated annotation) where
    fmap f (Annotated ann x) = Annotated ann (f x)

deriveGeneric (''Annotated)

rangesA :: NonEmpty (Annotated Range a) -> Range
rangesA = sconcat . fmap (.annotation)

data Literal
    = NumLit Double
    | StringLit Text
    deriving (Show, Eq, Typeable, Data)

deriveGeneric (''Literal)

newtype Ident = Ident Text
    deriving (Show, Eq, Typeable, Data)

deriveGeneric (''Ident)

newtype TypeIdent = TypeIdent Text
    deriving (Show, Eq, Typeable, Data)

deriveGeneric (''TypeIdent)

newtype FunctionParams = FunctionParams [ASTType]
    deriving (Show, Eq, Typeable, Data)
data ASTType
    = ASTType TypeIdent
    | FuncType FunctionParams ASTType
    deriving (Show, Eq, Typeable, Data)
deriveGeneric (''FunctionParams)
deriveGeneric (''ASTType)

data IdentWithType = IdentWithType Ident ASTType
    deriving (Show, Eq, Typeable, Data)
deriveGeneric (''IdentWithType)

data Expression annotation
    = ELiteral Literal
    | EIdentifier Ident
    | EList [Annotated annotation (Expression annotation)]
    | ECall
        (Annotated annotation (Expression annotation))
        [(Annotated annotation (Expression annotation))]
    deriving (Eq, Show, Typeable, Data)
deriveGeneric (''Expression)

data Pattern annotation
    = VarPattern (Annotated annotation Ident)
    | LitPattern Literal
    deriving (Eq, Show, Typeable, Data)
deriveGeneric (''Pattern)

data Statement annotation
    = Assignment (Annotated annotation Ident) (Annotated annotation (Expression annotation))
    | StatementExpression (Annotated annotation (Expression annotation))
    | If
        (Annotated annotation (Expression annotation))
        (NonEmpty (Annotated annotation (Statement annotation)))
        (NonEmpty (Annotated annotation (Statement annotation)))
    | FunDecl
        (Annotated annotation Ident)
        [Annotated annotation IdentWithType]
        (Annotated annotation ASTType)
        (NonEmpty (Annotated annotation (Statement annotation)))
    | MatchCase
        (Annotated annotation (Expression annotation))
        [Annotated annotation (Pattern annotation, NonEmpty (Annotated annotation (Statement annotation)))]
    deriving (Show, Eq, Typeable, Data)
deriveGeneric (''Statement)

type ParsedAST annotation = [Annotated annotation (Statement annotation)]

astMapM ::
    forall m d.
    (Monad m, Data d) =>
    -- | function that is run before descending a node
    (forall a. (Data a) => a -> m ()) ->
    -- | function that is run after modifying a node
    (forall a. (Data a) => a -> m ()) ->
    -- | modifier for nodes
    (forall a. (Data a) => a -> m a) ->
    d ->
    m d
astMapM fbefore fafter f tree = do
    fbefore tree
    changedTree <- gmapM (astMapM fbefore fafter f) tree >>= f
    fafter changedTree
    pure changedTree