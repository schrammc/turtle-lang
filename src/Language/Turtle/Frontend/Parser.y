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
    id       { Ranged { value = Identifier $$ } }
    num      { Ranged { value = TNumber $$ } }
    '='      { Ranged { value = TAssign } }
    ':'      { Ranged { value = TColon } }
    ','      { Ranged { value = TComma } }
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
Expression    : num { ELiteral (NumLit $1) }
              | Identifier { EIdentifier $1 }
Identifier    : id { Ident $1 }

{

happyError :: Alex a
happyError = Alex $ \alexState -> do 
  Left $ "Unspecified parser error at (char, line, col) "  ++ show alexState.alex_pos

}