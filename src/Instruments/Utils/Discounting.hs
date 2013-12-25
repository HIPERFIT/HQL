module Discounting where
import Prelude

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

-- TODO: Implement TermStructure to work on Payments (=[Payment])
discountPayments :: Date -> TermStructure -> Payments -> [Cash]
discountPayments = undefined
