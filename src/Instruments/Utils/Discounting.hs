-- module Discounting where
module Discounting where
import Interest
import Prelude
import Data.Char

-- Continuous discount factor p(t,T), T is fixed
--type DiscountFactor = DiscountFactor Double
    
-- |The 'discountFactor' function returns a continous discount factor
-- It takes two argument, the interest rate and the time to maturity
-- The last parameter is used for currying, but can be set to zero intially
--discountFactor :: InterestRate -> Rate -> DiscountFactor
discountFactor (InterestRate r) timeToMaturity t = (exp(-(r/100.0)*(timeToMaturity - t)))

-- |Returns a list of discount factors, this is the preferred way of usage
-- Returns a discount function, a list of discount factors to take an offset t
--discountFactors :: InterestRate -> t -> Rate -> [DiscountFactor]
discountFactors interest timeToMaturity length = 
    [discountFactor interest i| i <- [1..length]]

{-
Usage:
discountFactors (interestRate (Periodic 1) 5.0) 5 10 <*> pure 0

Annuity:
sum $ zipWith (*) (discountFactors (interestRate (Periodic 1) 5.0) 5 10 <*> pure 0) [1000,1000,1000,1000,1000]
4329.476670630818
-}
