{-# OPTIONS_GHC -Wno-orphans   #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module BezierSpec where

import Data.Bezier
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Linear.Vector
import Linear.V2
import Test.QuickCheck.Checkers

instance Arbitrary a => Arbitrary (V2 a) where
  arbitrary = V2 <$> arbitrary <*> arbitrary

instance EqProp (V2 Double)
instance EqProp (V2 Rational)

spec :: Spec
spec = do
  prop "endpoint @ 0 should be head ctrls" $ \(NonEmpty (ctrls :: [V2 Double])) -> do
    let bez = bezier ctrls
    runBezier bez 0 =-= head ctrls

  prop "endpoint @ 1 should be last ctrls" $ \(NonEmpty (ctrls :: [V2 Double])) -> do
    let bez = bezier ctrls
    runBezier bez 1 =-= last ctrls

  prop "constant" $ \(pt :: V2 Double) t -> do
    let bez = bezier [pt]
    runBezier bez t =-= pt

  prop "scale invariant" $ \(NonEmpty (ctrls :: [V2 Rational])) (a :: Rational) -> do
    runBezier (bezier $ fmap (^* a) ctrls) =-= (^* a) . runBezier (bezier ctrls)

  prop "position invariant" $ \(NonEmpty (ctrls :: [V2 Rational])) (pt :: V2 Rational) t -> do
    runBezier (bezier $ fmap (+ pt) ctrls) t =-= (+ pt) (runBezier (bezier ctrls) t)

  prop "invertible" $ \(NonEmpty (ctrls :: [V2 Rational])) t -> do
    runBezier (bezier ctrls) t =-= runBezier (bezier $ reverse ctrls) (1 - t)

