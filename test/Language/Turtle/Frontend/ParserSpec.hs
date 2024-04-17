module Language.Turtle.Frontend.ParserSpec where

import Language.Turtle.Frontend.Lexer (tokenize)
import Language.Turtle.Frontend.ParsedAST (Expression (..), Ident (..), Literal (..), ParsedAST (..), Statement (..))
import Language.Turtle.Frontend.Parser (program)

import Test.Hspec

spec :: Spec
spec = describe "Language.Turtle.Frontend.Parser" $ do
    it "basic program" $
        program <$> tokenize "a = 1" `shouldBe` Right [AStatement (Assignment (Ident "a") (ELiteral (NumLit 1)))]