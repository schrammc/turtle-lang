module Language.Turtle.Frontend.ParserSpec (spec) where

import Control.Monad (forM_)
import Data.Either (isLeft, isRight)
import qualified Data.Text.IO as T
import Language.Turtle.Frontend.Lexer (runAlexWithError)
import Language.Turtle.Frontend.ParsedAST (Expression (..), Ident (..), Literal (..), Statement (..))
import Language.Turtle.Frontend.Parser (program)
import Language.Turtle.Frontend.Range
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
    runAlexWithError fileContent program `shouldSatisfy` isRight

failureFiles :: Spec
failureFiles = filesInDirRecursive "test-files/parser/failure" $ \path -> do
    fileContent <- T.readFile path
    runAlexWithError fileContent program `shouldSatisfy` isLeft

spec :: Spec
spec = describe "Language.Turtle.Frontend.Parser" $ do
    successFiles
    failureFiles

    it "basic program" $
        runAlexWithError "a = 1" program
            `shouldBe` Right
                [ Ranged
                    { value =
                        Assignment
                            ( Ranged
                                { value = Ident "a"
                                , range =
                                    Range
                                        { start = Pos{line = 1, column = 1}
                                        , stop = Pos{line = 1, column = 2}
                                        }
                                }
                            )
                            ( Ranged
                                { value = ELiteral (NumLit 1.0)
                                , range =
                                    Range
                                        { start = Pos{line = 1, column = 5}
                                        , stop = Pos{line = 1, column = 6}
                                        }
                                }
                            )
                    , range =
                        Range
                            { start = Pos{line = 1, column = 1}
                            , stop = Pos{line = 1, column = 6}
                            }
                    }
                ]