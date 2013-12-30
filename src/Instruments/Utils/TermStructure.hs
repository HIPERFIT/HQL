{-# LANGUAGE RankNTypes #-}
module TermStructure where
import qualified Data.Map as M

type Rate = Double
type Rates = [Double]

data Compounding = Continuous | Periodic Int deriving (Show)

data InterestRates = InterestRate Compounding Rate
                   | InterestRates Compounding Rates
                   | TermStructureRates Compounding TermStructure

-- |Convert disscrete compounding to continous
mc 1 r = log(1+r/100.0)*100.0
mc p r = log((1.0+r/(100.0*pp))**pp)*100.0 where pp = fromIntegral p
makeContinuous (InterestRate (Periodic p) r) = InterestRate Continuous (mc p r)
makeContinuous (InterestRates (Periodic p) rs) = InterestRates Continuous (map (mc p) rs)

-- We don't use dates here?
-- Term structure/yield is a function of time to maturity (t :: Double)
type Points = M.Map Double Rate
type InterpolatedTermStructure = Points
type AnalyticalTermStructure = Double => Double

data TermStructure = Interpolated InterpolatedTermStructure
                   | Analytical AnalyticalTermStructure

instance Show TermStructure where
    show (Interpolated _) = "InterpolatedTermStructure"
    show (Analytical _) = "AnalyticalTermStructure"
            
-- |Create an analytic term structure
termStructure function = Analytical function
