{
module Language.Turtle.Frontend.Parser where

import Language.Turtle.Frontend.Lexer (Alex, Token(..), RangedToken(..), alexGetUserState, runAlex, lexwrap)
import Language.Turtle.Frontend.ParsedAST 
}

%name program Program
%tokentype { RangedToken }
%monad { Alex }
%lexer { lexwrap } {RangedToken { rtToken = EOF }}

%token
    id  { RangedToken { rtToken = Identifier $$ } }
    num { RangedToken { rtToken = TNumber $$ } }
    '=' { RangedToken { rtToken = TAssign } }
    eof { RangedToken { rtToken = EOF } }

%%
Program       : Statements { $1 }
Statements    : Statement { [ $1 ] }
              | Statements Statement { $2 : $1 }
Statement     : Identifier '=' Expression { AStatement (Assignment $1 $3) }
Expression    : num { ELiteral (NumLit $1) }
              | Identifier { EIdentifier $1 }
Identifier    : id { Ident $1 }
         
    

{

happyError :: Alex a
happyError = do 
  st <- alexGetUserState
  error $ "Unspecified parser error: " ++ show st

}