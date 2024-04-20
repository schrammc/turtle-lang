module Language.Turtle.Frontend.ParserSpec (spec) where

import Control.Monad (forM_)
import Data.Either (isRight)
import qualified Data.Text.IO as T
import Language.Turtle.Frontend.Lexer (runAlex)
import Language.Turtle.Frontend.ParsedAST (Expression (..), Ident (..), Literal (..), Statement (..))
import Language.Turtle.Frontend.Parser (program)
import System.Directory
import System.FilePath
import Test.Hspec

filesInDirRecursive :: FilePath -> (FilePath -> IO ()) -> Spec
filesInDirRecursive topDir toSpec = go topDir
  where
    go path = do
        isFile <- runIO $ doesFileExist path
        if isFile
            then it path (toSpec path)
            else do
                content <- runIO $ listDirectory path
                describe path (forM_ ((path </>) <$> content) go)

successFiles :: Spec
successFiles = filesInDirRecursive "test-files/parser/success" $ \path -> do
    fileContent <- T.readFile path
    runAlex fileContent program `shouldSatisfy` isRight

spec :: Spec
spec = describe "Language.Turtle.Frontend.Parser" $ do
    successFiles

    it "basic program" $
        runAlex "a = 1" program `shouldBe` Right [Assignment (Ident "a") (ELiteral (NumLit 1))]