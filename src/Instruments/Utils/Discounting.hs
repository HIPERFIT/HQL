-- module Discounting where
import Prelude

import qualified Data.Map as M
import Bonds
import TermStructure
import Currency
import Calendar

type Offset = Double
type DiscountFactor = Double

-- Continuous compounding factor p(t,T)
-- Zero coupon bond with nominal value N=1
--ContinuousDiscountFactor :: Double -> Double -> Double
--ContinuousDiscountFactor = 0

-- DiscountFactor is a curried function from [t,S,T]
-- where the future times S < T are prevailing at time t
-- Present value is represented as [0,0,T] (t=S)
-- Future value at time t=0 as [0,S,T]

-- No reason for using two offsets
discountFactor :: InterestRate -> Offset -> DiscountFactor
discountFactor (InterestRate Continuous r) tt = exp(-(r/100)*tt)
discountFactor (InterestRate (Periodic i) r) tt = ccf r tt i
  where ccf r t p = 1/(1 + (r/100)/p)**(p*t)

discountPayment :: InterestRate -> Date -> Payment -> Cash
discountPayment ir now (Payment date cash) = scale df cash
  where df = discountFactor ir $ getDayOffset date now

discountPayments :: Date -> TermStructure -> Payments -> [Cash]
discountPayments now (Analytical f) pms = zipWith df rates pms
  where rates = map (\(Payment d c) -> mkCont $ f $ getDayOffset now d) pms
        df    = \r p -> discountPayment r now p
        mkCont  = InterestRate Continuous
-- TODO: Intersection should match on compounding if there are multiple
-- interest rates for a given date
discountPayments now (Interpolated ts) pms
  | M.size ts >= length pms = M.elems $ M.intersectionWith df ts pmap
  | otherwise = []
  where pmap = M.fromList $ map (\p@(Payment d _) -> (d,p)) pms
        df   = \r p -> discountPayment r now p

-- Zero coupon yield
zcbYield :: Cash -> Cash -> Int -> Double
zcbYield face pv periods =
  let 
    (Cash v _) = expC (1/ fromIntegral periods) (face/pv) -- Checks currencies
  in
    v - 1

{- Yield tests

a = Cash 100 SEK
b = Cash  90 SEK
zcbYield a b 1 == 0.11111111111111116

c = Cash 500 USD
d = Cash 490 USD
zcbYield a b 3 == 0.006756961723555888

-}
