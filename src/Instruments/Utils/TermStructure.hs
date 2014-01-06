{-# LANGUAGE RankNTypes #-}
module Instruments.Utils.TermStructure
    (
    Compounding(..),
    InterestRate,
    InterestRates(..),
    TermStructure(..),
    Rate,
    Rates,
    makeContinuous,
    mc,
    ) where
import qualified Data.Map as M
import Utils.Calendar

type Rate = Double
type Rates = [Double]

-- TODO: LinearExponential
data Compounding = Continuous
                 | Linear Int 
                 | Exponential Int deriving (Show) 

type InterestRate = Rate

data InterestRates = InterestRate Compounding Rate
                   | InterestRates Compounding Rates
                   | TermStructureRates Compounding TermStructure

mc :: Int -> Rate -> Rate
-- TODO: Check this
--mc 1 r = log(1+r)
--mc p r = log((1.0+r/(100*pp))**pp) where pp = fromIntegral p

mc 1 r = log(1+r/100.0)*100.0
mc p r = (log((1.0+r/(100.0*pp))**pp))*100 where pp = fromIntegral p

-- |Convert discrete compounding to continous
makeContinuous :: Compounding -> Rate -> Rate
makeContinuous Continuous r = r
makeContinuous (Exponential n) r = mc n r
makeContinuous (Linear n) r = log $ r*n'  ** recip n' where n' = fromIntegral n

makePeriodic :: Compounding -> Rate -> Int -> Rate
makePeriodic Continuous r n = (exp(r/(nn*100))-1)*100*nn where nn = fromIntegral n

-- Better to return interest rate, otherwise we don't know for sure what it is... ie Rate is what?

type Points = M.Map Date Rate -- Assumes zero rates
type InterpolatedTermStructure = Points
type AnalyticalTermStructure = Double -> Double

data TermStructure = Interpolated InterpolatedTermStructure
                   | Analytical AnalyticalTermStructure

instance Show TermStructure where
    show (Interpolated _) = "InterpolatedTermStructure"
    show (Analytical _) = "AnalyticalTermStructure"
            
-- |Create an analytic term structure
termStructure :: AnalyticalTermStructure -> TermStructure
termStructure = Analytical
