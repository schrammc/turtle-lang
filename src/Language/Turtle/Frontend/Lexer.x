{
{-# LANGUAGE MultiWayIf #-}
module Language.Turtle.Frontend.Lexer 
  ( Token(..)
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
import Language.Turtle.Frontend.Range (Pos(Pos), Range(..), Ranged(..))

}

%wrapper "monadUserState-strict-text"

$digit = [0-9]
$alpha = [a-zA-Z]
$alphaLower = [a-z]
$alphaUpper = [A-Z]
$underscore = \_
$dot = \.

@arrow = "->"
@identifier = ($alphaLower | $underscore) ($alpha | $digit | $underscore)*
@upperIdentifier = $alphaUpper ($alpha | $digit | $underscore)*
@digits = $digit+
@number = (\-?) @digits ($dot $digit+)?



tokens :-

<0> @identifier           { nameCaptureToken Identifier }
<0> @upperIdentifier      { nameCaptureToken TUpperIdentifier }
<0> @number               { numberToken }
<0> @arrow                { aToken TArrow }
<0> \n [\ ]*              { indentToken }
<0> [=]                   { aToken TAssign }
<0> [\ \t]                ;
<0> :                     { aToken TColon }
<0> \,                    { aToken TComma }
<0> \"                    { begin string }
<0> [\(\[\[\)\]\}]        { paren}

<string> [^\"]*           { stringLitToken }
<string> \"               { begin 0 }

{
data Token 
  = Identifier Text 
  | TUpperIdentifier Text
  | TIndent Int 
  | TArrow
  | TUnindent
  | TNewline
  | TAssign
  | TStringLit Text 
  | TNumber Double 
  | TColon
  | TComma
  | TParenL
  | TParenR
  | TBracketL
  | TBracketR
  | TBraceL
  | TBraceR
  | TIf
  | TElse
  | TFun
  | EOF
  deriving (Eq, Show)

data AlexUserState = AlexUserState
  { indentLevels :: [Int]
  , pendingTokens :: [Ranged Token]
  , parenStack :: [Char]
  }
  deriving (Eq, Show)

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState [] [] []

alexModifyUserState :: (AlexUserState -> AlexUserState) -> Alex ()
alexModifyUserState f = alexSetUserState . f =<< alexGetUserState

alexEOF :: Alex (Maybe (Ranged Token))
alexEOF = do
  (pos, _, _, _) <- alexGetInput
  st <- alexGetUserState
  let makeToken t = Ranged t (Range (fromAlexPos pos) (fromAlexPos pos))
      unindents = makeToken . const TUnindent <$> st.indentLevels 
  alexSetUserState st { indentLevels = []
                      , pendingTokens = st.pendingTokens ++ unindents ++ [makeToken EOF ]
                      }
  pure $ Nothing


fromAlexPos :: AlexPosn -> Pos
fromAlexPos (AlexPn _ line_ column_) = Pos line_ column_

mkRange :: AlexInput -> Int -> Range
mkRange (start_, _, _, str) len = 
  Range { start = fromAlexPos start_
        , stop = fromAlexPos stop_
        }
  where
    stop_ = T.foldl' alexMove start_ $ T.take len str

aToken :: Token -> AlexAction (Maybe (Ranged Token))
aToken = nameCaptureToken . const

nameCaptureToken :: (Text -> Token) -> AlexAction (Maybe (Ranged Token))
nameCaptureToken buildToken inp@(_, _, _, str) len =
  pure $ Just $ Ranged
    { value = tok
    , range = mkRange inp len
    }
  where
    idStr = T.take len str
    tok = 
      case idStr of 
        "if"  -> TIf
        "else" -> TElse
        "fun" -> TFun
        _ -> buildToken idStr

numberToken ::  AlexAction (Maybe (Ranged Token))
numberToken  inp@(_, _, _, str) len =
  pure $ Just $ Ranged
    { value = TNumber $ read $ T.unpack $ T.take len str
    , range = mkRange inp len
    }

paren :: AlexAction (Maybe (Ranged Token))
paren inp@(_,_,_,str) len
  | ch `elem` openParens = do 
      alexModifyUserState (\s -> s { parenStack = ch:s.parenStack})
      pure $ Just rangedT
  | ch `elem` closeParens = do 
      st <- alexGetUserState
      case st.parenStack of 
        (p:ps) | match p ch -> do 
                  alexSetUserState st { parenStack = ps } 
                  pure $ Just rangedT
               | otherwise  -> fail $ "Lexer error: mismatched '" ++ [ch] ++ "'"
        [] -> fail "Lexer error: internal error (paren, 1)"
  | otherwise = fail $ "Lexer error: internal error (paren, unrecognized char) '" ++ ch:"'"
  where
    ch = T.head str
    openParens = "([{" :: [Char]
    closeParens = ")]}" :: [Char]
    match '(' ')' = True
    match '[' ']' = True
    match '{' '}' = True 
    match _   _   = False
    rangedT = Ranged { value = token_, range = mkRange inp len }
    token_ = 
      case ch of 
        '(' -> TParenL
        ')' -> TParenR
        '[' -> TBracketL
        ']' -> TBracketR
        '{' -> TBraceL
        '}' -> TBraceR
        _ -> error "Internal lexer error"

instance MonadFail Alex where 
  fail msg = Alex (const $ Left msg)

indentToken ::  AlexAction (Maybe (Ranged Token))
indentToken inp@(_, _, _, str) len =  
  if | T.null rest -> pure Nothing 
     | T.head rest == '\n' -> pure Nothing 
     | otherwise -> do
         st <- alexGetUserState
         let indentLevel = len - 1
         case (indentLevel, st.indentLevels, st.parenStack) of
           (_, _, (_:_)) -> pure Nothing
           (0, [], []) -> pure $ Just $ mkToken TNewline
           (x, [], []) -> indentTo x
           (x, (y:ys), []) | x == y -> pure $ Just $ mkToken TNewline
                       | x > y -> indentTo x
                       | otherwise -> do
                         let (unindentedLevels, restLevels) = span (/= x) ys
                             unindentToken = mkToken TUnindent
                             pendingUnindents = replicate (length unindentedLevels) unindentToken
                             emitTokens = do
                               when (length unindentedLevels > 0) $ 
                                 alexModifyUserState $ \s -> s { pendingTokens = s.pendingTokens ++ pendingUnindents}
                               pure $ Just unindentToken
                         
                         case restLevels of 
                             [] | x == 0 -> do
                                  alexModifyUserState $ \s -> s { indentLevels = restLevels}
                                  emitTokens
                                | otherwise -> Alex $ const $ Left "Bad unindent"
                             (a:_) | a /= x -> Alex $ const $ Left "Bad unindent"
                                   | otherwise -> do 
                                       alexModifyUserState $ \s -> s { indentLevels = restLevels }
                                       emitTokens
  where
    (_, rest) = T.splitAt len str
    mkToken t = Ranged
            { value = t
            , range = mkRange inp len
            }
    indentTo x = do      
      alexModifyUserState $ \st -> st{ indentLevels = (x:st.indentLevels)}
      pure $ Just $ mkToken $ TIndent (len - 1)

                  

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
        (tok:xs) -> do 
          alexSetUserState st { pendingTokens = xs }
          if value tok == EOF
            then pure [tok]
            else (tok :) <$> go

-- The idea here is that the whole setup is geared toward emitting one token at
-- a time. Unfortunately when unindenting it can happen that we undindent out 
-- of a number of blocks that we want to emit multiple unindent tokens in one
-- go.
--
-- This is where the hack with pending tokens comes from, even though the
-- mechanism as it's implemented here is more general and could be used for
-- other things as well.
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