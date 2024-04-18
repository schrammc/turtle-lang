{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import qualified Data.Text.IO as T
import Language.Turtle.Frontend.Lexer
import Language.Turtle.Frontend.Parser
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
    let parseResult = runAlex fileContent program
    case parseResult of
        Right res -> print res
        Left err -> do
            putStrLn $ "ERROR: " ++ err
            exitFailure