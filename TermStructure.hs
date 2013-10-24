{-# LANGUAGE GADTs, RankNTypes #-}
--module TermStructure where

import qualified Data.Time as D
import qualified Data.Map as M

-- Same types used here as we discusses
type Date = D.UTCTime
type Points = M.Map Date Double 

-- TODO: return Cash instead of Num, use currencies etc.

type FlatTermStructure = forall f. (Floating f) => f
type InterpolatedTermStructure = Points
type AnalyticalTermStructure = forall f. (Floating f) => Int -> f

data TermStructure = Flat         FlatTermStructure
                   | Interpolated InterpolatedTermStructure
                   | Analytical   AnalyticalTermStructure

{-
class TermStructure t where
    discountD :: Floating f => t -> f
    discountT :: Floating f => t -> f
--    discountD :: (Num a) => t -> a -> a
   -- discountT :: (Num a) => t -> a -> a -- The discount factor for a given time (t in days)
    --discountD :: (Date d, Num a) => d -> a
    --discountT :: (Num a) => a -> a

instance TermStructure Flat where
    discountD = constantYield -- Just a flat/constant yield
    discountT = constantYield -- Just a flat/constant yield

-- Interpolated term structure, interpolating between known points (yield/date pairs)
instance TermStructure Interpolated where
    discountD d = 1.0 -- TODO: lookup and interpolate (use Interpolation class?)
    discountT t = 1.0 -- TODO: lookup and interpolate (use Interpolation class?)

-- Analytical term strucutre, using an analytical function
instance TermStructure Analytical where
    discountD d = 5.0 + sqrt(1.0) * 0.25 -- Example from D.E. documentation -- TODO: convert d (date) to time
    discountT t = 5.0 + sqrt(1.0) * 0.25 -- Example from D.E. documentation

-}
