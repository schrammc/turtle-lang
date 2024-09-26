{
{-# LANGUAGE FlexibleContexts #-}
module Language.Turtle.Frontend.Parser where

import Language.Turtle.Frontend.Lexer (Alex(..), AlexState(..), Token(..), alexGetUserState, lexwrap)
import Language.Turtle.Frontend.Range (Ranged(..), Range(..), ranges)
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
    '|'      { Ranged { value = TPipe } }
    if       { Ranged { value = TIf } }
    else     { Ranged { value = TElse } }
    match    { Ranged { value = TMatch } }
    case     { Ranged { value = TCase } }
    eof      { Ranged { value = EOF } }
    fun      { Ranged { value = TFun } }
    indent   { Ranged { value = TIndent _ } }
    unindent { Ranged { value = TUnindent } }
    newline  { Ranged { value = TNewline } }

%left '|'
%left '->'

%%
Program       : Statements { $1 :: ParsedAST Range }
Block         : indent StatementsNonEmpty unindent { $2 }
BlockOrSingleStatement 
  : Block { $1 :: NonEmpty (Annotated Range (Statement Range))}
  | Statement newline { $1 :| [] }
Statements    
  : Statement { [ $1 ] }
  | Statement newline Statements { $1 : $3 }
StatementsNonEmpty 
  : Statement { $1 :| []}
  | Statement newline StatementsNonEmpty { $1 `NE.cons` $3 }

Statement :: { Annotated Range (Statement Range) }
Statement     
  : Identifier '=' Expression { Annotated ($1.annotation <> $3.annotation) (Assignment $1 $3)  }
  | if Expression ':' BlockOrSingleStatement else ':' BlockOrSingleStatement { Annotated ($2.annotation <> rangesA $4 <> rangesA $7) (If $2 $4 $7)  }
  | Expression { Annotated $1.annotation (StatementExpression $1)  }
  | fun Identifier paren_enclosed('(', ')', IdentifierWithType) '->' Type ':' BlockOrSingleStatement { Annotated ($2.annotation) (FunDecl $2 $3.annotatedValue $5 $7) }
  | CaseSt { $1 }

CaseSt
  : match Expression ':' indent Cases unindent { Annotated ($1.range <> $6.range) (MatchCase $2 $5)  }

Cases
  : Case { [ $1 ] }
  | Case Cases {$1 : $2 }

Case
  : case Pattern ':' BlockOrSingleStatement {Annotated ($1.range <> rangesA $4) ($2.annotatedValue, $4) }

Pattern :: { Annotated Range (Pattern Range) }
Pattern
  : num {let TNumber num = $1.value in Annotated $1.range (LitPattern (NumLit num)) }
  | Identifier { Annotated $1.annotation (VarPattern $1)  }

Expression :: { Annotated Range (Expression Range) }
Expression    
  : Literal { ELiteral `fmap` $1 }
  | Identifier { EIdentifier `fmap` $1 }
  | '(' Expression ')' { $2 }
  | Expression paren_enclosed('(', ')', Expression) { Annotated ($1.annotation <> $2.annotation) (ECall $1 $2.annotatedValue ) }
  | list_like(Expression) { EList `fmap` $1 }

Literal :: { Annotated Range Literal }
Literal
  : num { let TNumber num = $1.value in Annotated $1.range (NumLit num) }
  | stringlit { let TStringLit str = $1.value in Annotated $1.range (StringLit str) }
    
IdentifierWithType
  : Identifier ':' Type { Annotated ($1.annotation <> $3.annotation) (IdentWithType $1.annotatedValue $3.annotatedValue)  }

SimpleType : TypeIdent { ASTType `fmap` $1 }

Type 
  : SimpleType { $1 }
  | FunctionParams '->' Type  { Annotated ($1.annotation <> $3.annotation) (FuncType $1.annotatedValue $3.annotatedValue) }
  | Type '|' Type { Annotated ($1.annotation <> $3.annotation) (Union $1.annotatedValue $3.annotatedValue)}
  | SimpleType SimpleType { Annotated ($1.annotation <> $2.annotation) (TypeApplication $1.annotatedValue $2.annotatedValue)}
  | SimpleType '(' Type ')' { Annotated ($1.annotation <> $4.range) (TypeApplication $1.annotatedValue $3.annotatedValue)}


FunctionParams
  : paren_enclosed('(', ')', Type) { (FunctionParams . (fmap (.annotatedValue))) `fmap` $1 }

Identifier    
  : id { let Identifier val = $1.value in Annotated $1.range (Ident val) }
TypeIdent 
  : Id { let TUpperIdentifier val = $1.value in Annotated $1.range (TypeIdent val) }


list_like(p)
  : paren_enclosed('[', ']', p) { $1 }

paren_enclosed(l,r,p)
  : l r { Annotated ($1.range <> $2.range) [] }
  | l comma_separated(p) r { Annotated ($1.range <> $3.range) $2  }

comma_separated(p)
  : p ',' comma_separated(p) { $1 : $3 }
  | p { [$1] }
                  
{

parserErrorHandler :: (Ranged Token, [String]) -> Alex a
parserErrorHandler (_, possibleTokens) = fail $ "Parser error. Possible tokens:\n    - " ++ intercalate "\n    - " possibleTokens 

}