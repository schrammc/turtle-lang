module Language.Turtle.Location (Location (..)) where

import Language.Turtle.Frontend.Range (Range)

data Location = Location
    { locationFile :: FilePath
    , locationRange :: Range
    }
    deriving (Show, Eq)