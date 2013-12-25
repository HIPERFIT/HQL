{-# LANGUAGE RankNTypes #-}
module TermStructure where
import qualified Data.Map as M
import Calendar

type Points = M.Map Date Double 

type InterpolatedTermStructure = Points
type AnalyticalTermStructure = forall f. (Floating f) => Int -> f

data TermStructure = Interpolated InterpolatedTermStructure
                   | Analytical   AnalyticalTermStructure
