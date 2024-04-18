{
module Language.Turtle.Frontend.Lexer 
  ( Token(..)
  , Ranged(..)
  , Alex
  , alexGetUserState
  , runAlex
  , lexwrap
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
  deriving (Eq, Show)

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState

alexEOF :: Alex (Ranged Token)
alexEOF = do
  (pos, _, _, _) <- alexGetInput
  pure $ Ranged EOF (Range pos pos)

data Range = Range
  { start :: AlexPosn
  , stop :: AlexPosn
  } deriving (Eq, Show)

data Ranged a = Ranged
  { value :: a
  , range :: Range
  } deriving (Eq, Show)

mkRange :: AlexInput -> Int -> Range
mkRange (start_, _, _, str) len = Range{start = start_, stop = stop_}
  where
    stop_ = T.foldl' alexMove start_ $ T.take len str

aToken :: Token -> AlexAction (Ranged Token)
aToken = nameCaptureToken . const

nameCaptureToken :: (Text -> Token) -> AlexAction (Ranged Token)
nameCaptureToken buildToken inp@(_, _, _, str) len =
  pure Ranged
    { value = buildToken $ T.take len str
    , range = mkRange inp len
    }

numberToken ::  AlexAction (Ranged Token)
numberToken  inp@(_, _, _, str) len =
  pure Ranged
    { value = TNumber $ read $ T.unpack $ T.take len str
    , range = mkRange inp len
    }

indentToken ::  AlexAction(Ranged Token)
indentToken  inp len =
  pure Ranged
    { value = TIndent (len - 1)
    , range = mkRange inp len
    }

stringLitToken ::  AlexAction (Ranged Token)
stringLitToken  inp@(_, _, _, str) len =
  pure Ranged
    { value = TStringLit $ T.take len str
    , range = mkRange inp len
    }

tokenize :: Text -> Either String [Ranged Token]
tokenize input = runAlex input go
  where
    go = do
      output <- alexMonadScan
      if value output == EOF
        then pure [output]
        else (output :) <$> go

lexwrap :: (Ranged Token -> Alex a) -> Alex a
lexwrap cont = alexMonadScan >>= cont
}