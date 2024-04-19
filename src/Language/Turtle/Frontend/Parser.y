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
    id  { Ranged { value = Identifier $$ } }
    num { Ranged { value = TNumber $$ } }
    '=' { Ranged { value = TAssign } }
    eof { Ranged { value = EOF } }

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
happyError = Alex $ \alexState -> do 
  Left $ "Unspecified parser error at (char, line, col)"  ++ show alexState.alex_pos

}