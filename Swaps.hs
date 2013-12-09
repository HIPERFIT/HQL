module Swaps where
import Bonds
import Calendar

data Swap = Swap 
data FixedLeg = FixedLeg Cashflow
data FloatingLeg = FloatingLeg Cashflow

swap :: (Bond b0, Bond b1) => b0 -> b1 -> Date -> Swap
swap = undefined
