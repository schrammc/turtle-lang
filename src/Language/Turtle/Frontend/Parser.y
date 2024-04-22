{
module Language.Turtle.Frontend.Parser where

import Language.Turtle.Frontend.Lexer (Alex(..), AlexState(..), Token(..), Ranged(..), alexGetUserState, runAlex, lexwrap)
import Language.Turtle.Frontend.ParsedAST 
}

%name program Program
%tokentype { Ranged Token }
%monad { Alex }
%lexer { lexwrap } {Ranged { value = EOF }}

%token
    id       { Ranged { value = Identifier _ } }
    num      { Ranged { value = TNumber num } }
    '='      { Ranged { value = TAssign } }
    ':'      { Ranged { value = TColon } }
    ','      { Ranged { value = TComma } }
    '('      { Ranged { value = TParenL } }
    ')'      { Ranged { value = TParenR } }
    if       { Ranged { value = TIf } }
    else     { Ranged { value = TElse } }
    eof      { Ranged { value = EOF } }
    indent   { Ranged { value = TIndent _ } }
    unindent { Ranged { value = TUnindent } }
    newline { Ranged { value = TNewline } }

%%
Program       : Statements { $1 }
Block         : indent Statements unindent { $2 }
BlockOrSingleStatement 
  : Block { $1 }
  | Statement { [ $1] }
Statements    : Statement { [ $1 ] }
              | Statement newline Statements { $1 : $3 }
Statement     : Identifier '=' Expression { (Assignment $1 $3) }
              | if Expression ':' BlockOrSingleStatement else ':' BlockOrSingleStatement { If $2 $4 $7 }
              | Expression { StatementExpression $1 }
Expression    : num { let TNumber num = $1.value in ELiteral (NumLit num) }
              | Identifier { EIdentifier $1 }
              | '(' Expression ')' { $2 }
Identifier    : id { let Identifier val = $1.value in Ident val }

{

happyError :: Alex a
happyError = Alex $ \alexState -> do 
  Left $ "Unspecified parser error at (char, line, col) "  ++ show alexState.alex_pos

}