module Language.Turtle.Frontend.ParserSpec (spec) where

import Language.Turtle.Frontend.Lexer (runAlex)
import Language.Turtle.Frontend.ParsedAST (Expression (..), Ident (..), Literal (..), ParsedAST (..), Statement (..))
import Language.Turtle.Frontend.Parser (program)

import Test.Hspec

spec :: Spec
spec = describe "Language.Turtle.Frontend.Parser" $ do
    it "basic program" $
        runAlex "a = 1" program `shouldBe` Right [AStatement (Assignment (Ident "a") (ELiteral (NumLit 1)))]