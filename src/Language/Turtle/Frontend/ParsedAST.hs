module Language.Turtle.Frontend.ParsedAST (
    ParsedAST,
    Literal (..),
    Statement (..),
    Ident (..),
    Expression (..),
) where

import Data.Text (Text)

newtype Ident = Ident Text
    deriving (Show, Eq)

type ParsedAST = [Statement]

data Expression
    = ELiteral Literal
    | EIdentifier Ident
    deriving (Show, Eq)

newtype Literal = NumLit Double
    deriving (Show, Eq)

data Statement
    = Assignment Ident Expression
    | StatementExpression Expression
    | If Expression [Statement] [Statement]
    deriving (Show, Eq)