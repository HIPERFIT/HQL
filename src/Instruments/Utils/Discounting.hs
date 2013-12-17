module Discounting where

import Bonds
import TermStructure
import Currency
import Calendar
type DiscountFactor = Double

-- Computes the discount factor
df :: InterestRate -> Years -> DiscountFactor
df = \r n -> 1.0 / (1.0 + (r / 100.0)) ** n

-- Computes a list of discount factors
dfs :: TermStructure -> [Years] -> [DiscountFactor]
dfs (Flat r) offsets = map (df r) offsets
dfs (Analytical _) _ = undefined
dfs (Interpolated ts) offsets = zipWith df ts offsets -- TODO: match on dates?

discountPayment :: Date -> InterestRate -> Payment -> Cash
discountPayment now r (Payment date cash) = scale (df r y) cash
  where y = getDayOffset now date

discountPayments :: Date -> TermStructure -> Payments -> [Cash]
discountPayments now ts pms = zipWith (discountPayment now) myDfs pms
  where offsets = map (\(Payment d _) -> getDayOffset now d) pms
        myDfs = dfs ts offsets
