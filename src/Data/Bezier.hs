module Data.Bezier
  ( bezier
  , Bezier(..)
  ) where

import Data.List (intersperse)

newtype Bezier a b = Bezier
  { runBezier :: a -> b
  }
  deriving newtype (Functor, Applicative, Monad, Semigroup, Monoid)

instance (Fractional a, Eq b) => Eq (Bezier a b) where
  Bezier x == Bezier y = and
    [ x 0 == y 0
    , x 0.5 == y 0.5
    , x 1 == y 1
    ]

instance (Fractional a, Show b) => Show (Bezier a b) where
  show (Bezier x) = mconcat
    [ "Bezier<"
    , unwords $ intersperse "," $ fmap show
        [ x 0
        , x 0.5
        , x 1
        ]
    , ">"
    ]


line1d :: Num a => a -> a -> Bezier a a
line1d a b = Bezier $ \t -> a * (1 - t) + b * t


line
    :: (Num a, Applicative f, Traversable f)
    => f a
    -> f a
    -> Bezier a (f a)
line a b = sequenceA $ liftA2 line1d a b


bezier
    :: (Num a, Applicative f, Traversable f)
    => [f a]
    -> Bezier a (f a)
bezier [] = error "bezier called on an empty list"
bezier [a] = pure a
bezier as = bezier =<< sequenceA (zipWith line as $ drop 1 as)

