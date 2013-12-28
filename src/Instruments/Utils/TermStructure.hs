{-# LANGUAGE RankNTypes #-}
module TermStructure where
import qualified Data.Map as M
import Calendar
import Data.Char

data InterestRate = InterestRate Compounding Double

-- data Compounding = Continuous | Periodic Int deriving (Show)
instance Show InterestRate where
  show (InterestRate c r) = show r ++ "% " ++ map toLower (show c) ++ " compounded"

type Points = M.Map Date InterestRate 
type InterpolatedTermStructure = Points
type AnalyticalTermStructure = forall f. (Floating f) => Double -> f

data TermStructure = Interpolated InterpolatedTermStructure
                   | Analytical   AnalyticalTermStructure
