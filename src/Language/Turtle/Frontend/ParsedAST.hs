module Language.Turtle.Frontend.ParsedAST (
    ParsedAST (..),
    Literal (..),
    Statement (..),
    Ident (..),
    Expression (..),
) where

import Data.Text (Text)

newtype Ident = Ident Text
    deriving (Show, Eq)

newtype ParsedAST
    = AStatement Statement
    deriving (Show, Eq)

data Expression
    = ELiteral Literal
    | EIdentifier Ident
    deriving (Show, Eq)

newtype Literal = NumLit Double
    deriving (Show, Eq)

data Statement
    = Assignment Ident Expression
    | StatementExpression Expression
    deriving (Show, Eq)