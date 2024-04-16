{
module Language.Turtle.Frontend.Lexer 
  ( Token(..)
  , RangedToken(..)
  , tokenize 
  ) where

import qualified Data.Text as T
import Data.Text (Text)

}

%wrapper "monadUserState-strict-text"

$digit = [0-9]
$alpha = [a-zA-Z]
$underscore = \_
$dot = \.

@identifier = ($alpha | $underscore) ($alpha | $digit | $underscore)*
@digits = $digit+
@number = (\-?) @digits ($dot $digit+)?



tokens :-

<0> @identifier { nameCaptureToken Identifier }
<0> @number     { numberToken }
<0> \n [\ ]*    { indentToken }
<0> [=]         { aToken TAssign }
<0> [\ \t] ; 
<0>      \"             { begin string }
<string> [^\"]*         { stringLitToken }
<string> \"             { begin 0 }

{
data Token 
  = Identifier Text 
  | TIndent Int 
  | TAssign
  | TStringLit Text 
  | TNumber Double 
  | EOF
  deriving (Eq, Show)

data AlexUserState = AlexUserState
  {
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState

alexEOF :: Alex RangedToken
alexEOF = do
  (pos, _, _, _) <- alexGetInput
  pure $ RangedToken EOF (Range pos pos)

data Range = Range
  { start :: AlexPosn
  , stop :: AlexPosn
  } deriving (Eq, Show)

mkRange :: AlexInput -> Int -> Range
mkRange (start_, _, _, str) len = Range{start = start_, stop = stop_}
  where
    stop_ = T.foldl' alexMove start_ $ T.take len str

data RangedToken = RangedToken
  { rtToken :: Token
  , rtRange :: Range
  } deriving (Eq, Show)

aToken :: Token -> AlexAction RangedToken
aToken = nameCaptureToken . const

nameCaptureToken :: (Text -> Token) -> AlexAction RangedToken
nameCaptureToken buildToken inp@(_, _, _, str) len =
  pure RangedToken
    { rtToken = buildToken $ T.take len str
    , rtRange = mkRange inp len
    }

numberToken ::  AlexAction RangedToken
numberToken  inp@(_, _, _, str) len =
  pure RangedToken
    { rtToken = TNumber $ read $ T.unpack $ T.take len str
    , rtRange = mkRange inp len
    }

indentToken ::  AlexAction RangedToken
indentToken  inp@(_, _, _, str) len =
  pure RangedToken
    { rtToken = TIndent (len - 1)
    , rtRange = mkRange inp len
    }

stringLitToken ::  AlexAction RangedToken
stringLitToken  inp@(_, _, _, str) len =
  pure RangedToken
    { rtToken = TStringLit $ T.take len str
    , rtRange = mkRange inp len
    }

tokenize :: Text -> Either String [RangedToken]
tokenize input = runAlex input go
  where
    go = do
      output <- alexMonadScan
      if rtToken output == EOF
        then pure [output]
        else (output :) <$> go
}