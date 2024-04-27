{
{-# LANGUAGE FlexibleContexts #-}
module Language.Turtle.Frontend.Parser where

import Language.Turtle.Frontend.Lexer (Alex(..), AlexState(..), Token(..), alexGetUserState, runAlex, lexwrap)
import Language.Turtle.Frontend.Range (Ranged(..), ranges)
import Language.Turtle.Frontend.ParsedAST 
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (sconcat)
import qualified Data.List.NonEmpty as NE

}

%name program Program
%tokentype { Ranged Token }
%monad { Alex }
%lexer { lexwrap } {Ranged { value = EOF }}
%errorhandlertype explist
%error { parserErrorHandler }

%token
    id       { Ranged { value = Identifier _ } }
    num      { Ranged { value = TNumber num } }
    '='      { Ranged { value = TAssign } }
    ':'      { Ranged { value = TColon } }
    ','      { Ranged { value = TComma } }
    '('      { Ranged { value = TParenL } }
    ')'      { Ranged { value = TParenR } }
    '['      { Ranged { value = TBracketL } }
    ']'      { Ranged { value = TBracketR } }
    if       { Ranged { value = TIf } }
    else     { Ranged { value = TElse } }
    eof      { Ranged { value = EOF } }
    indent   { Ranged { value = TIndent _ } }
    unindent { Ranged { value = TUnindent } }
    newline { Ranged { value = TNewline } }

%%
Program       : Statements { $1 :: [Ranged (Statement Ranged)] }
Block         : indent StatementsNonEmpty unindent { $2 }
BlockOrSingleStatement 
  : Block { $1 :: NonEmpty (Ranged (Statement Ranged))}
  | Statement { $1 :| [] }
Statements    
  : Statement { [ $1 ] }
  | Statement newline Statements { $1 : $3 }
StatementsNonEmpty 
  : Statement { $1 :| []}
  | Statement newline StatementsNonEmpty { $1 `NE.cons` $3 }
Statement     
  : Identifier '=' Expression { Ranged (Assignment $1 $3) ($1.range <> $3.range) }
  | if Expression ':' BlockOrSingleStatement else ':' BlockOrSingleStatement { Ranged (If $2 $4 $7) ($2.range <> ranges $4 <> ranges $7) }
  | Expression { Ranged (StatementExpression $1) $1.range }
Expression    
  : num { let TNumber num = $1.value in Ranged (ELiteral (NumLit num)) $1.range }
  | Identifier { EIdentifier `fmap` $1 }
  | '(' Expression ')' { $2 }
  | list_like(Expression) { EList `fmap` $1 }
Identifier    
  : id { let Identifier val = $1.value in Ranged (Ident val) $1.range }

list_like(p)
  : '[' ']' { Ranged [] ($1.range <> $2.range) }
  | '[' comma_separated(p) ']' { Ranged $2 ($1.range <> $3.range) }

comma_separated(p)
  : p ',' comma_separated(p) { $1 : $3 }
  | p { [$1] }
                  
{

parserErrorHandler :: (Ranged Token, [String]) -> Alex a
parserErrorHandler possibleTokens = Alex $ \alexState -> do 
  Left $ "Unspecified parser error at (char, line, col) "  ++ show alexState.alex_pos ++ "\npossible: " ++ show possibleTokens

}