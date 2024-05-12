{
{-# LANGUAGE FlexibleContexts #-}
module Language.Turtle.Frontend.Parser where

import Language.Turtle.Frontend.Lexer (Alex(..), AlexState(..), Token(..), alexGetUserState, lexwrap)
import Language.Turtle.Frontend.Range (Ranged(..), ranges)
import Language.Turtle.Frontend.ParsedAST 
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (sconcat)
import qualified Data.List.NonEmpty as NE
import Data.List (intercalate)

}

%name program Program
%tokentype { Ranged Token }
%monad { Alex }
%lexer { lexwrap } {Ranged { value = EOF }}
%errorhandlertype explist
%error { parserErrorHandler }

%token
    id       { Ranged { value = Identifier _ } }
    Id       { Ranged { value = TUpperIdentifier _ }}
    stringlit{ Ranged { value = TStringLit _ }}
    num      { Ranged { value = TNumber num } }
    '='      { Ranged { value = TAssign } }
    ':'      { Ranged { value = TColon } }
    ','      { Ranged { value = TComma } }
    '('      { Ranged { value = TParenL } }
    ')'      { Ranged { value = TParenR } }
    '['      { Ranged { value = TBracketL } }
    ']'      { Ranged { value = TBracketR } }
    '->'     { Ranged { value = TArrow } }
    if       { Ranged { value = TIf } }
    else     { Ranged { value = TElse } }
    match    { Ranged { value = TMatch } }
    case     { Ranged { value = TCase } }
    eof      { Ranged { value = EOF } }
    fun      { Ranged { value = TFun } }
    indent   { Ranged { value = TIndent _ } }
    unindent { Ranged { value = TUnindent } }
    newline  { Ranged { value = TNewline } }

%%
Program       : Statements { $1 :: [Ranged (Statement Ranged)] }
Block         : indent StatementsNonEmpty unindent { $2 }
BlockOrSingleStatement 
  : Block { $1 :: NonEmpty (Ranged (Statement Ranged))}
  | Statement newline { $1 :| [] }
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
  | fun Identifier paren_enclosed('(', ')', IdentifierWithType) '->' Type ':' BlockOrSingleStatement { Ranged (FunDecl $2 $3.value $5 $7) ($2.range) }
  | CaseSt { $1 }

CaseSt
  : match Expression ':' indent Cases unindent { Ranged (MatchCase $2 $5) ($1.range <> $6.range) }

Cases
  : Case { [ $1 ] }
  | Case Cases {$1 : $2 }

Case
  : case Pattern ':' BlockOrSingleStatement {Ranged ($2.value, $4) ($1.range <> ranges $4)}

Pattern
  : num {let TNumber num = $1.value in Ranged (LitPattern (NumLit num)) $1.range}
  | Identifier { Ranged (VarPattern $1) $1.range }

Expression    
  : Literal { ELiteral `fmap` $1 }
  | Identifier { EIdentifier `fmap` $1 }
  | '(' Expression ')' { $2 }
  | Expression paren_enclosed('(', ')', Expression) { Ranged (ECall $1 $2.value ) ($1.range <> $2.range)}
  | list_like(Expression) { EList `fmap` $1 }

Literal
  : num { let TNumber num = $1.value in Ranged (NumLit num) $1.range }
  | stringlit { let TStringLit str = $1.value in Ranged (StringLit str) $1.range }
    
IdentifierWithType
  : Identifier ':' Type { Ranged (IdentWithType $1.value $3.value) ($1.range <> $3.range) }

Type 
  : TypeIdent { ASTType `fmap` $1 }
  | FunctionParams '->' Type  { Ranged (FuncType $1.value $3.value) ($1.range <> $3.range)}

FunctionParams
  : paren_enclosed('(', ')', Type) { (FunctionParams . (fmap (.value))) `fmap` $1 }

Identifier    
  : id { let Identifier val = $1.value in Ranged (Ident val) $1.range }
TypeIdent 
  : Id { let TUpperIdentifier val = $1.value in Ranged (TypeIdent val) $1.range }


list_like(p)
  : paren_enclosed('[', ']', p) { $1 }

paren_enclosed(l,r,p)
  : l r { Ranged [] ($1.range <> $2.range) }
  | l comma_separated(p) r { Ranged $2 ($1.range <> $3.range) }

comma_separated(p)
  : p ',' comma_separated(p) { $1 : $3 }
  | p { [$1] }
                  
{

parserErrorHandler :: (Ranged Token, [String]) -> Alex a
parserErrorHandler (_, possibleTokens) = fail $ "Parser error. Possible tokens:\n    - " ++ intercalate "\n    - " possibleTokens 

}