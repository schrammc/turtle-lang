{
{-# LANGUAGE MultiWayIf #-}
module Language.Turtle.Frontend.Lexer 
  ( Token(..)
  , Ranged(..)
  , Alex(..)
  , AlexState(..)
  , alexGetUserState
  , runAlex
  , lexwrap
  , tokenize 
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import Control.Monad (when)

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
<0> [\ \t]      ;
<0> :           { aToken TColon }
<0> \,          { aToken TComma }
<0> \"          { begin string }
<0> if          { aToken TIf}
<0> else        { aToken TElse}
<string> [^\"]* { stringLitToken }
<string> \"     { begin 0 }

{
data Token 
  = Identifier Text 
  | TIndent Int 
  | TUnindent
  | TAssign
  | TStringLit Text 
  | TNumber Double 
  | TColon
  | TComma
  | TIf
  | TElse
  | EOF
  deriving (Eq, Show)

data AlexUserState = AlexUserState
  { indentLevels :: [Int]
  , pendingTokens :: [Ranged Token]
  }
  deriving (Eq, Show)

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState [] []

alexModifyUserState :: (AlexUserState -> AlexUserState) -> Alex ()
alexModifyUserState f = alexSetUserState . f =<< alexGetUserState

alexEOF :: Alex (Maybe (Ranged Token))
alexEOF = do
  (pos, _, _, _) <- alexGetInput
  pure $ Just $ Ranged EOF (Range pos pos)

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

aToken :: Token -> AlexAction (Maybe (Ranged Token))
aToken = nameCaptureToken . const

nameCaptureToken :: (Text -> Token) -> AlexAction (Maybe (Ranged Token))
nameCaptureToken buildToken inp@(_, _, _, str) len =
  pure $ Just $ Ranged
    { value = buildToken $ T.take len str
    , range = mkRange inp len
    }

numberToken ::  AlexAction (Maybe (Ranged Token))
numberToken  inp@(_, _, _, str) len =
  pure $ Just $ Ranged
    { value = TNumber $ read $ T.unpack $ T.take len str
    , range = mkRange inp len
    }

indentToken ::  AlexAction (Maybe (Ranged Token))
indentToken inp@(_, _, _, str) len =  
  if | T.null rest -> pure Nothing 
     | T.head rest == '\n' -> pure Nothing 
     | otherwise -> do
         st <- alexGetUserState
         let indentLevel = len - 1
         case (indentLevel, st.indentLevels) of
           (0, []) -> pure Nothing
           (x, []) -> indentTo x
           (x, (y:ys)) | x == y -> pure Nothing
                       | x > y -> indentTo x
                       | otherwise -> do
                         let (unindentedLevels, restLevels) = span (/= x) ys
                             unindentToken = Ranged { value = TUnindent, range = mkRange inp len}
                             pendingUnindents = replicate (length unindentedLevels) unindentToken
                             emitTokens = do
                               when (length unindentedLevels > 0) $ 
                                 alexModifyUserState $ \s -> s { pendingTokens = s.pendingTokens ++ pendingUnindents}
                               pure $ Just unindentToken
                         case restLevels of 
                             [] | x == 0 -> emitTokens
                                | otherwise -> Alex $ const $ Left "Bad unindent"
                             _ -> do
                               alexModifyUserState $ \s -> s { indentLevels = restLevels }
                               emitTokens
  where
    (_, rest) = T.splitAt len str
    indentTo x = do      
      alexModifyUserState $ \st -> st{ indentLevels = (x:st.indentLevels)}
      pure $ Just $ Ranged
            { value = TIndent (len - 1)
            , range = mkRange inp len
            }

                  

stringLitToken ::  AlexAction (Maybe (Ranged Token))
stringLitToken  inp@(_, _, _, str) len =
  pure $ Just $ Ranged
    { value = TStringLit $ T.take len str
    , range = mkRange inp len
    }

tokenize :: Text -> Either String [Ranged Token]
tokenize input = runAlex input go
  where
    go = do
      st <- alexGetUserState
      case st.pendingTokens of 
        [] -> do
            output <- alexMonadScan
            case output of 
              Nothing -> go
              Just tok -> 
                if value tok == EOF
                  then pure [tok]
                  else (tok :) <$> go
        (x:xs) -> do 
          alexSetUserState st { pendingTokens = xs }
          (x:) <$> go

lexwrap :: (Ranged Token -> Alex a) -> Alex a
lexwrap cont = do 
  st <- alexGetUserState
  case st.pendingTokens of 
    [] -> alexMonadScan >>= \case 
            Nothing -> lexwrap cont
            Just tok -> cont tok
    (x:xs) -> do 
      alexSetUserState st { pendingTokens = xs }
      cont x
}