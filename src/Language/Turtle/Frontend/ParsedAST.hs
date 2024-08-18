{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
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
) where

import Data.Functor.Classes (Eq1, Show1)
import Data.Kind (Type)
import Data.List.NonEmpty
import Data.Text (Text)
import GHC.Generics (Generic)
import Generics.SOP.TH

data Literal
    = NumLit Double
    | StringLit Text
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
    | ECall (f (Expression f)) [f (Expression f)]
    deriving (Generic)

deriveGeneric (''Expression)

class HFunctor t f where
    ffmap :: (Functor f) => (forall a. f a -> g a) -> t f -> t g

mapffmap :: (Functor f, HFunctor t f) => (forall a. f a -> g a) -> f (t f) -> g (t g)
mapffmap f x = f $ ffmap f <$> x

instance HFunctor Expression a where
    ffmap _ (ELiteral x) = ELiteral x
    ffmap _ (EIdentifier x) = EIdentifier x
    ffmap f (EList xs) = EList $ mapffmap f <$> xs
    ffmap f (ECall fun args) = ECall (mapffmap f fun) (mapffmap f <$> args)

deriving instance (Show1 f) => Show (Expression f)
deriving instance (Eq1 f) => Eq (Expression f)

data Pattern (f :: Type -> Type)
    = VarPattern (f Ident)
    | LitPattern Literal

instance HFunctor Pattern a where
    ffmap f (VarPattern x) = VarPattern (f x)
    ffmap _ (LitPattern l) = LitPattern l

deriveGeneric (''Pattern)

deriving instance (Show1 f) => Show (Pattern f)
deriving instance (Eq1 f) => Eq (Pattern f)

data Statement (f :: Type -> Type)
    = Assignment (f Ident) (f (Expression f))
    | StatementExpression (f (Expression f))
    | If (f (Expression f)) (NonEmpty (f (Statement f))) (NonEmpty (f (Statement f)))
    | FunDecl (f Ident) [f IdentWithType] (f ASTType) (NonEmpty (f (Statement f)))
    | MatchCase (f (Expression f)) [f (Pattern f, NonEmpty (f (Statement f)))]

deriveGeneric (''Statement)

instance HFunctor Statement a where
    ffmap f (Assignment i e) = Assignment (f i) (mapffmap f e)
    ffmap f (StatementExpression e) = StatementExpression (mapffmap f e)
    ffmap f (If cond tru fal) = If (mapffmap f cond) (mapffmap f <$> tru) (mapffmap f <$> fal)
    ffmap f (FunDecl name args ftype body) = FunDecl (f name) (f <$> args) (f ftype) (mapffmap f <$> body)
    ffmap f (MatchCase matched cases) = MatchCase (mapffmap f matched) $ do
        branch <- cases
        let bla (pat, body) = (ffmap f pat, mapffmap f <$> body)
        pure $ f $ bla <$> branch

deriving instance (Show1 f) => Show (Statement f)
deriving instance (Eq1 f) => Eq (Statement f)

type ParsedAST (f :: Type -> Type) = [f (Statement f)]