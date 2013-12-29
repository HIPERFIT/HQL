{-# LANGUAGE RankNTypes #-}
module TermStructure where
--import Calendar
import Interest
import qualified Data.Map as M
import Data.Char

-- We don't use dates here?
-- Term structure/yield is a function of time to maturity (t :: Double)
type Points = M.Map Double InterestRate 
type InterpolatedTermStructure = Points
type AnalyticalTermStructure = Double => Double

data TermStructure = Interpolated InterpolatedTermStructure
                   | Analytical AnalyticalTermStructure

instance Show TermStructure where
    show (Interpolated _) = "InterpolatedTermStructure"
    show (Analytical _) = "AnalyticalTermStructure"
            
-- |Create an analytic term structure
termStructure function = Analytical function
