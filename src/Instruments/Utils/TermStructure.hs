{-# LANGUAGE RankNTypes #-}
module Instruments.Utils.TermStructure where
import qualified Data.Map as M
import Utils.Calendar
import Data.Number.CReal

type Rate = CReal
type Rates = [CReal]

-- TODO: LinearExponential
data Compounding = Continuous
                 | Linear Int 
                 | Exponential Int deriving (Show) 

type InterestRate = Rate

mc :: Int -> Rate -> Rate
-- TODO: Check this
mc 1 r = log(1+r)
mc p r = log((1.0+r/(100*pp))**pp) where pp = fromIntegral p

-- |Convert discrete compounding to continous
makeContinuous :: Compounding -> Rate -> Rate
makeContinuous Continuous r = r
makeContinuous (Exponential n) r = mc n r
makeContinuous (Linear n) r = log $ r*n'  ** recip n' where n' = fromIntegral n

type Points = M.Map Date Rate -- Assumes zero rates
type InterpolatedTermStructure = Points
type AnalyticalTermStructure = CReal -> CReal

data TermStructure = Interpolated InterpolatedTermStructure
                   | Analytical AnalyticalTermStructure

instance Show TermStructure where
    show (Interpolated _) = "InterpolatedTermStructure"
    show (Analytical _) = "AnalyticalTermStructure"
            
-- |Create an analytic term structure
termStructure :: AnalyticalTermStructure -> TermStructure
termStructure = Analytical
