{-# LANGUAGE InstanceSigs #-}

module Language.Turtle.Frontend.Range (Pos (..), Range (..)) where

data Pos = Pos {line :: Int, column :: Int}
    deriving (Eq, Ord, Show)

data Range = Range
    { start :: Pos
    , stop :: Pos
    }
    deriving (Eq, Show)

instance Semigroup Range where
    (<>) :: Range -> Range -> Range
    Range start1 stop1 <> Range start2 stop2 = Range (min start1 start2) (max stop1 stop2)