{-# LANGUAGE RankNTypes #-}
module TermStructure where
import qualified Data.Map as M
import Calendar

-- Same types used here as we discusses
-- type Date = D.Day
type Points = M.Map Date Double 

type FlatTermStructure = forall f. (Floating f) => f
type InterpolatedTermStructure = Points
type AnalyticalTermStructure = forall f. (Floating f) => Int -> f

data TermStructure = Flat         FlatTermStructure
                   | Interpolated InterpolatedTermStructure
                   | Analytical   AnalyticalTermStructure
