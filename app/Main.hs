{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Data.Text as T
import qualified Data.Text.IO as T
import Language.Turtle.Error (TurtleError (..), parserError, renderErrorWithFileContent)
import Language.Turtle.Frontend.Lexer
import Language.Turtle.Frontend.Parser
import Language.Turtle.Location (Location (..))
import Options.Applicative
import System.Exit (exitFailure)

data ParseOptions = ParseOptions {filePath :: String}

data Command = Parse ParseOptions

parseCommand :: Parser Command
parseCommand = Parse . ParseOptions <$> argument str (metavar "FILE")

oparse :: ParserInfo Command
oparse =
    info
        ( helper
            <*> subparser
                ( command "parse" (info parseCommand (progDesc "Parse some turtle-lang code"))
                )
        )
        (fullDesc <> progDesc "The Turtle programming language" <> header "turtle")

main :: IO ()
main = do
    execParser oparse >>= \case
        Parse opts -> runParse opts

runParse :: ParseOptions -> IO ()
runParse opts = do
    fileContent <- T.readFile $ opts.filePath
    let parseResult = runAlexWithError fileContent program
    case parseResult of
        Right res -> print res
        Left (Left msg) -> putStrLn msg >> exitFailure
        Left (Right (range, err)) -> do
            let turtleError =
                    TurtleError
                        { errorId = parserError
                        , message = T.pack $ err
                        , location = Location opts.filePath range
                        }
            T.putStrLn $ renderErrorWithFileContent turtleError fileContent
            exitFailure