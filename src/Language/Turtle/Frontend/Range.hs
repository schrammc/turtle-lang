{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Turtle.Frontend.Range (Pos (..), Range (..), Ranged (..), (<*|>), ranges) where

import Data.Functor.Classes
import Data.List.NonEmpty (NonEmpty (..))

data Ranged a = Ranged
    { value :: a
    , range :: Range
    }
    deriving (Eq, Show)

deriving instance Functor Ranged
instance Show1 Ranged where
    liftShowsPrec sp _ n (Ranged x r) = ("Ranged " <>) . sp n x . showsPrec n r
instance Eq1 Ranged where
    liftEq eq (Ranged x1 r1) (Ranged x2 r2) = eq x1 x2 && r1 == r2

ranges :: NonEmpty (Ranged a) -> Range
ranges (x :| xs) = foldr (\(Ranged _ r) acc -> r <> acc) (range x) xs

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