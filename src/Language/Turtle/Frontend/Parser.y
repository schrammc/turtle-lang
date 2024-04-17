{
module Language.Turtle.Frontend.Parser where

import Language.Turtle.Frontend.Lexer (Token(..), RangedToken(..))
import Language.Turtle.Frontend.ParsedAST 
}

%name program Statements
%tokentype { RangedToken }
%error { parseError }

%token
    id  { RangedToken { rtToken = Identifier $$ } }
    num { RangedToken { rtToken = TNumber $$ } }
    '=' { RangedToken { rtToken = TAssign } }

%%
Statements    : Statement { [ $1 ] }
              | Statements Statement { $2 : $1 }
Statement     : Identifier '=' Expression { AStatement (Assignment $1 $3) }
Expression    : num { ELiteral (NumLit $1) }
              | Identifier { EIdentifier $1 }
Identifier    : id { Ident $1 }
         
    

{

parseError :: [RangedToken] -> a
parseError _ = error "Parse error"

}