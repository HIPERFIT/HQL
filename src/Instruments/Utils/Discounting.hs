-- |
-- Module:      Instruments.Utils.Discounting
-- Copyright:   (c) 2013 HIPERFIT
-- License:     BSD-3
-- Maintainer:  Johan Astborg <joastbg@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working with discounting
-- and time value of money.

module Instruments.Utils.Discounting where
--import Instruments.Utils.TermStructure
--import Instruments.Utils.InterestRate

--import qualified Data.Map as M

{-
-- Round a number f to n number of digits
--rnd :: Integer -> Double -> Double
rnd n f = fromInteger (round $ f * (10 ^ nn)) / (10.0 ^^ nn)
	where nn = max 0 (n - floor(logBase 10 f) - 1)

-- |Discount Factors
)

df' t maturityT r = exp(-(r/100.0)*(maturityT - t))

discountFactor' :: Double -> Double -> Double -> Double -> Double
discountFactor' r maturityT t d = df' t maturityT r
-}

---- NEW CODE
{-
type AnalyticalTermStructure = Double -> Double

data TermStructure = Analytical AnalyticalTermStructure

instance Show TermStructure where
    show (Analytical _) = "AnalyticalTermStructure"
            
-- |Create an analytic term structure
termStructure :: AnalyticalTermStructure -> TermStructure
termStructure = Analytical
-}

--type Points = M.Map Date ContinuousInterestRate -- Assumes zero rates
--type InterpolatedTermStructure = Double -> ContinuousInterestRate

type DiscountFactor = Double -> Double -> Double

continuousDf offsetT maturityT r = exp(-(r/100.0)*(maturityT - offsetT))

-- | Represents a single discount factor in continuous time
--
--discountFactor :: Double -> Double -> Double -> Double -> Double
--discountFactor r maturityT t d = continuousDf t maturityT r

-- | Represents a lazy list of discount factors in continuous time
--

-- Continuous discount factor p(t,T), T is fixed

--InterestRate

--interestRate -> discountFactor
--termStructure -> discountFactor

--discountFactor interestRate offsetT maturityT
--discountFactor termStructure offsetT maturityT

--discountFactors :: InterestRate -> Double -> [Double]

--discountFactors interestRate offsetT = 
--  zipWith (continuousDf offsetT) [(offsetT+1)..] (repeat 0)

{-
discountFactors' :: TermStructure -> Double -> [Double]
discountFactors' (Analytical f) offsetT = 
  map (\t ->continuousDf offsetT t (rate (discountFactor (f t)))) [offsetT+1-delta]
  where delta = 0.0 -- TODO: Daycount conventions
-}


-- Forward rate

-- take 5 $ discountFactors (InterestRate 10.4713 Compounded Monthly) 0
-- take 10 $ discountFactors' (Analytical (\t -> ContinuousInterestRate 5.0)) 0.0

-- |Lazy lists Discount Factors from rate, rates of term structure.
-- will repeat the input sequence to infinity
{-
discountFactors' :: InterestRates -> Rate -> Rate -> [Rate]

discountFactors' (InterestRate Continuous r) offsetT  delta=
  zipWith (df' offsetT) [(offsetT+1-delta)..] (repeat r)

discountFactors' (InterestRates Continuous rs) offsetT delta=
  zipWith (df' offsetT) [offsetT+1..] (cycle rs)

discountFactors' (TermStructureRates Continuous (Analytical ts)) offsetT  delta=
  map (\t ->discountFactor' (ts t) t offsetT delta) [offsetT+1..]

discountFactors' (InterestRate (Exponential p) r) offsetT delta =
  discountFactors' (InterestRate Continuous (mc p r)) offsetT delta

discountFactors' (InterestRates (Exponential p) rs) offsetT delta =
  discountFactors' (InterestRates Continuous (map (mc p) rs)) offsetT delta

discountFactors' (TermStructureRates (Exponential p) (Analytical ts)) offsetT delta =
  map (\t ->discountFactor' (mc p (ts t)) t offsetT delta) [offsetT+(1/pp)..]
  where pp = fromIntegral p


-- Interface for use externally
discountFactors :: Compounding -> Date -> TermStructure -> [Date] -> [Double]
discountFactors c now (Analytical f) ds =
  zipWith df offsets rates
  where offsets = map (getYearOffset now) ds
        rates = map (makeContinuous c . recip . f) offsets
discountFactors c now (Interpolated ts) ds =
  let
    offsets = map (getYearOffset now) ds -- TODO: Wrap in ErrorT monad
    rates = map (\d -> makeContinuous c $ ts M.! d) ds
  in
    zipWith df offsets rates
-}
-- Tests


-- NTest 11
--ts1  = Analytical analyticalFun1
--now1 = read "2000-01-01" :: Date
--mat1 = read "2004-07-05" :: Date
--cmp1 = Exponential 2
--df1  = discountFactors cmp1 now1 ts1 [mat1] -- TODO: Conversion is buggy,
                                            -- check `mc` in TermStructure

-- NTest 15
--mat2 = read "2000-07-02" :: Date
--ts2  = Interpolated $ M.fromList [(mat2, 0.083)]
--cmp2 = Continuous
--df2  = discountFactors cmp2 now1 ts2 [mat2]

-- tests = [df1 == 0.7643885607510086,
--tests = [head df2 == 0.9593493353414723]

--testAll = all (==True) tests
--
--


-- $use
-- 
-- This section contains basic information about using the
-- discount factor and related functions defined in this module.
