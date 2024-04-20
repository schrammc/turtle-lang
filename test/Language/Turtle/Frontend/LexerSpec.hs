module Language.Turtle.Frontend.LexerSpec (spec) where

import Data.Text (Text)
import Language.Turtle.Frontend.Lexer (Token (..), tokenize, value)
import Test.Hspec

shouldTokenizeAs :: Text -> [Token] -> Expectation
shouldTokenizeAs input expected = do
  fmap value <$> tokenize input `shouldBe` Right expected

spec :: Spec
spec = do
  describe "Lexer" $ do
    describe "identifier" $ do
      it "a" $ shouldTokenizeAs "a" [Identifier "a", EOF]
      it "a_" $ shouldTokenizeAs "a_" [Identifier "a_", EOF]
      it "_a" $ shouldTokenizeAs "_a" [Identifier "_a", EOF]

    describe "linebreaks / empty lines" $ do
      it "a\\nb" $ shouldTokenizeAs "a\nb" [Identifier "a", Identifier "b", EOF]
      it "a\\n\\nb" $ shouldTokenizeAs "a\n\nb" [Identifier "a", Identifier "b", EOF]
      it "a\\n  \t\\nb" $ shouldTokenizeAs "a\n\nb" [Identifier "a", Identifier "b", EOF]

    describe "indent" $ do
      it "a\\n  b" $ shouldTokenizeAs "a\n  b" [Identifier "a", TIndent 2, Identifier "b", TUnindent, EOF]
      it "a\\n \\n  b" $ shouldTokenizeAs "a\n \n  b" [Identifier "a", TIndent 2, Identifier "b", TUnindent, EOF]
      it "a\\n  b\\n    c" $ shouldTokenizeAs "a\n  b\n    c" [Identifier "a", TIndent 2, Identifier "b", TIndent 4, Identifier "c", TUnindent, TUnindent, EOF]
      it "a\\n  b\\n    c\\nd" $
        shouldTokenizeAs
          "a\n  b\n    c\nd"
          [ Identifier "a"
          , TIndent 2
          , Identifier "b"
          , TIndent 4
          , Identifier "c"
          , TUnindent
          , TUnindent
          , Identifier "d"
          , EOF
          ]
      it "a\\n  b\\n    c\\n      d\\n  e" $
        shouldTokenizeAs
          "a\n  b\n    c\n      d\n  e"
          [ Identifier "a"
          , TIndent 2
          , Identifier "b"
          , TIndent 4
          , Identifier "c"
          , TIndent 6
          , Identifier "d"
          , TUnindent
          , TUnindent
          , Identifier "e"
          , TUnindent
          , EOF
          ]
      it "a\\n  b\\nc" $ shouldTokenizeAs "a\n  b\nc" [Identifier "a", TIndent 2, Identifier "b", TUnindent, Identifier "c", EOF]

    describe "String Literal" $ do
      xit "\"\"" $ shouldTokenizeAs "\"\"" [TStringLit "", EOF]
      it "\"abc\"" $ shouldTokenizeAs "\"abc\"" [TStringLit "abc", EOF]

    describe "Numbers" $ do
      it "123" $ shouldTokenizeAs "123" [TNumber 123, EOF]
      it "-123" $ shouldTokenizeAs "-123" [TNumber (-123), EOF]
      it "-123.456" $ shouldTokenizeAs "-123.456" [TNumber (-123.456), EOF]
      it "0" $ shouldTokenizeAs "0" [TNumber 0, EOF]
      it "-0" $ shouldTokenizeAs "-0" [TNumber 0, EOF]
      it "0.0" $ shouldTokenizeAs "0.0" [TNumber 0.0, EOF]
