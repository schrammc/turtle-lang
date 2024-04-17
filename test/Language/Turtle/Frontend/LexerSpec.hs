module Language.Turtle.Frontend.LexerSpec where

import Test.Hspec
import Language.Turtle.Frontend.Lexer (tokenize, Token(..), rtToken)

spec :: Spec
spec = do
  describe "Lexer" $ do
    it "should tokenize a simple case" $ do
        let input = "a b"
        fmap rtToken <$> tokenize input `shouldBe` Right [Identifier "a", Identifier "b", EOF]
