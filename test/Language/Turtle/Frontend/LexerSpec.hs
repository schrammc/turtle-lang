module Language.Turtle.Frontend.LexerSpec where

import Data.Text (Text)
import Language.Turtle.Frontend.Lexer (Token (..), rtToken, tokenize)
import Test.Hspec

shouldTokenizeAs :: Text -> [Token] -> Expectation
shouldTokenizeAs input expected = do
  fmap rtToken <$> tokenize input `shouldBe` Right expected

spec :: Spec
spec = do
  describe "Lexer" $ do
    describe "identifier" $ do
      it "a" $ shouldTokenizeAs "a" [Identifier "a", EOF]
      it "a_" $ shouldTokenizeAs "a_" [Identifier "a_", EOF]
      it "_a" $ shouldTokenizeAs "_a" [Identifier "_a", EOF]
    describe "String Literal" $ do
      it "\"\"" $ shouldTokenizeAs "\"\"" [TStringLit "", EOF]
      it "\"abc\"" $ shouldTokenizeAs "\"abc\"" [TStringLit "abc", EOF]

    describe "Numbers" $ do
      it "123" $ shouldTokenizeAs "123" [TNumber 123, EOF]
      it "-123" $ shouldTokenizeAs "-123" [TNumber (-123), EOF]
      it "-123.456" $ shouldTokenizeAs "-123.456" [TNumber (-123.456), EOF]
      it "0" $ shouldTokenizeAs "0" [TNumber 0, EOF]
      it "-0" $ shouldTokenizeAs "-0" [TNumber 0, EOF]
      it "0.0" $ shouldTokenizeAs "0.0" [TNumber 0.0, EOF]
