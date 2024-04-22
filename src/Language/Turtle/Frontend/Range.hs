{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Turtle.Frontend.Range (Pos (..), Range (..), Ranged (..), (<*|>)) where

data Ranged a = Ranged
    { value :: a
    , range :: Range
    }
    deriving (Eq, Show)

deriving instance Functor Ranged

(<*|>) :: Ranged (a -> b) -> Ranged a -> Ranged b
f <*|> a = Ranged (f.value a.value) (f.range <> a.range)

data Pos = Pos {line :: Int, column :: Int}
    deriving (Eq, Ord, Show)

data Range = Range
    { start :: Pos
    , stop :: Pos
    }
    deriving (Eq, Show)

instance Semigroup Range where
    Range start1 stop1 <> Range start2 stop2 = Range (min start1 start2) (max stop1 stop2)