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

<0> $white+ ;
<0> @identifier { nameCaptureToken Identifier }
<0> @number { numberToken }
<0> \s { ignoreToken }

{
data Token = Identifier Text | TIndent Int | TNumber Double | EOF
  deriving (Eq, Show)

data AlexUserState = AlexUserState
  {
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState

alexEOF :: Alex RangedToken
alexEOF = do
  (pos, _, _, _) <- alexGetInput
  pure $ RangedToken (Just EOF) (Range pos pos)

data Range = Range
  { start :: AlexPosn
  , stop :: AlexPosn
  } deriving (Eq, Show)

mkRange :: AlexInput -> Int -> Range
mkRange (start_, _, _, str) len = Range{start = start_, stop = stop_}
  where
    stop_ = T.foldl' alexMove start_ $ T.take len str

data RangedToken = RangedToken
  { rtToken :: Maybe Token
  , rtRange :: Range
  } deriving (Eq, Show)

nameCaptureToken :: (Text -> Token) -> AlexAction RangedToken
nameCaptureToken buildToken inp@(_, _, _, str) len =
  pure RangedToken
    { rtToken = Just $ buildToken $ T.take len str
    , rtRange = mkRange inp len
    }

numberToken ::  AlexAction RangedToken
numberToken  inp@(_, _, _, str) len =
  pure RangedToken
    { rtToken = Just $ TNumber $ read $ T.unpack $ T.take len str
    , rtRange = mkRange inp len
    }

indentToken ::  AlexAction RangedToken
indentToken  inp@(_, _, _, str) len =
  pure RangedToken
    { rtToken = Just $ TIndent len
    , rtRange = mkRange inp len
    }

ignoreToken ::  AlexAction RangedToken
ignoreToken  inp len =
  pure RangedToken
    { rtToken = Nothing
    , rtRange = mkRange inp len
    }

tokenize :: Text -> Either String [RangedToken]
tokenize input = runAlex input go
  where
    go = do
      output <- alexMonadScan
      if rtToken output == Just EOF
        then pure [output]
        else (output :) <$> go
}