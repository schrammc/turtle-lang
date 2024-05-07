{-# LANGUAGE DuplicateRecordFields #-}

module Language.Turtle.Error (TurtleError (..), renderError, parserError, renderErrorWithFileContent) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Turtle.Frontend.Range (Pos (..), Range (..))
import Language.Turtle.Location (Location (..))

data ErrorId = ErrorId {errorId :: Int, errorShortName :: Text}
    deriving (Show)

formatErrorId :: ErrorId -> Text
formatErrorId eid = "[" <> eid.errorShortName <> "]"

parserError :: ErrorId
parserError = ErrorId 1 "parser-error"

data TurtleError
    = TurtleError
    { errorId :: ErrorId
    , message :: Text
    , location :: Location
    }
    deriving (Show)

renderError :: (MonadIO m) => TurtleError -> m Text
renderError err = do
    fileContent <- liftIO $ T.readFile err.location.locationFile
    pure $ renderErrorWithFileContent err fileContent

renderErrorWithFileContent :: TurtleError -> Text -> Text
renderErrorWithFileContent err fileContent =
    let Pos{line, column} = err.location.locationRange.start
        errorLine = case drop (line - 1) (T.lines fileContent) of
            [] -> error "Internal error: renderError - line out of bounds"
            (x : _) -> x
        spaces = take (column - 1) (repeat ' ')
        caret = T.pack $ spaces ++ "^"
        indent = ("    " <>)
     in T.unlines
            [ "Error: " <> formatErrorId err.errorId <> " " <> T.pack (err.location.locationFile <> ":")
            , T.unlines $ indent <$> [errorLine, caret] ++ T.lines (message err)
            ]