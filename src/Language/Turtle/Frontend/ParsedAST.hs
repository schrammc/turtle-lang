{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Turtle.Frontend.ParsedAST (
    ParsedAST,
    Literal (..),
    Statement (..),
    Ident (..),
    Expression (..),
) where

import Data.Functor.Classes (Eq1, Show1)
import Data.Kind (Type)
import Data.List.NonEmpty
import Data.Text (Text)

newtype Ident = Ident Text
    deriving (Show, Eq)

type ParsedAST (f :: Type -> Type) = [f (Statement f)]

data Expression (f :: Type -> Type)
    = ELiteral Literal
    | EIdentifier Ident

deriving instance (Show1 f) => Show (Expression f)
deriving instance (Eq1 f) => Eq (Expression f)
newtype Literal = NumLit Double
    deriving (Show, Eq)

data Statement (f :: Type -> Type)
    = Assignment (f Ident) (f (Expression f))
    | StatementExpression (f (Expression f))
    | If (f (Expression f)) (NonEmpty (f (Statement f))) (NonEmpty (f (Statement f)))

deriving instance (Show1 f) => Show (Statement f)
deriving instance (Eq1 f) => Eq (Statement f)