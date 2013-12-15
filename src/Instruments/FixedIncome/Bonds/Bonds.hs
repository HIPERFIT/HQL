{-# LANGUAGE RecordWildCards #-}
module Bonds where
import qualified Data.Map as M
import qualified Data.List as L
import Calendar
import Currency
import TermStructure
import Control.Monad
import Prelude

--
-- Types
--

type InterestRate = Double
type Repayment = Double

data Basis = ACTACT | ACT360 | ACT365F | Thirty360 | SIA | Business | European | Japanese

data Payment = Payment Date Cash deriving (Show)
type Payments = [Payment]
type Cashflow = Payments
type DiscountFunction = TermStructure -> Years -> Double
-- type DiscountFunction = Date -> Payment -> Payment

--
-- Instruments
--

newtype Vanilla = Vanilla { payments :: Payments }
newtype Zero   = Zero { payment :: Payment } deriving (Show)
newtype Consol = Consol { cpayments :: Payments }
newtype Bullet = Bullet { bpayments :: Payments }
newtype Annuity = Annuity { apayments :: Payments }
newtype Serial = Serial { spayments :: Payments }
data Compounding = Continous | Periodic Int

data BaseBond = BaseBond {
  settle       :: Date,           -- Settlement date
  currency     :: Currency,       -- Currency
  basematurity :: Date,           -- Maturity date
  couponRate   :: InterestRate,   -- Coupon rate
  period       :: Int,            -- Settlements per year
  basis        :: Basis,          -- Day-count basis
  roll         :: RollConvention, -- End-of-month rule
  faceValue    :: Double,         -- Face value of bond
  compounding  :: Compounding,    -- Coupons per year (default = 2)
  couponRates  :: [Double]        -- Coupon rate ???
}

zero :: BaseBond -> Zero
zero BaseBond{..} = Zero $ Payment basematurity $ Cash faceValue currency 

consol :: BaseBond -> Consol
consol BaseBond{..} =
  let
    mkPayment date = Payment date $ Cash (faceValue * couponRate) currency
    dates = getSettlementDates roll period settle
  in
    Consol $ map mkPayment dates

bullet :: BaseBond -> Bullet
bullet BaseBond{..} = 
  let
    mkPayment date = Payment date $ Cash (faceValue * couponRate) currency 
    dates = getSettlementDates roll period settle
  in
    Bullet $ map mkPayment dates

-- Serial takes a BaseBond as well as a fixed repayment
serial :: Repayment -> BaseBond -> Serial
serial repayment BaseBond{..} =
  let
    dates = getSettlementDates roll period settle
    fv' = Cash faceValue currency 
    accPymts (Cash outstd currency) date = let
                                 coupon  = couponRate * outstd
                                 outstd' = outstd - (coupon + repayment)
                               in (Cash outstd' currency, Payment date $ Cash coupon currency)
    (remaining, payments) = L.mapAccumL accPymts fv' dates
  in
    Serial $ payments ++ [Payment basematurity remaining] 

--
-- Classes
-- 

class Instrument i where
  pv :: i -> InterestRate -> Date -> Cash
  expired  :: i -> Date -> Bool
  cashflow :: i -> Payments
  yrsToExpiry :: i -> Date -> Years

class Instrument b => Bond b where
  principal :: b -> Cash
  coupon    :: b -> Cash
  maturity  :: b -> Date
  ytm       :: b -> Date -> Payment
  duration  :: b -> Payment
--   clean :: b -> ... -> Cash 
--   dirty :: b -> ... -> Payment

class Instrument d => Derivative d where
  underlying :: (Instrument i) => d -> i

-- class (Instrument e) => Equity e where
-- class (Instrument o) => Option o where

{-
> df :: TermStructure -> Years -> Double
> df (Flat r) yrs = 1.0 / (1.0 + (r / 100.0)) ** yrs

Take DiscountFunction, use currying from discount function
PartialDiscountFunction
> pv' :: (Years -> Double) -> Date -> Payment -> Double
> pv' df now (Payment date (Cash v _)) = (df yrs) * v
>   where yrs = (fromIntegral $ Calendar.diffDays date now) / 365.0 -- TODO: leap year
 
> yrs' now date = (fromIntegral $ Calendar.diffDays date now) / 365.0
 
> rate1 = Flat 12
> now  = (read "2011-01-01") :: Date
> d1   = (read "2011-12-31") :: Date -- 1 yrs from spot
> d2   = (read "2012-12-31") :: Date -- 2 yrs from spot
> d3   = (read "2013-12-31") :: Date -- 3 yrs from spot
> d4   = (read "2014-12-31") :: Date -- 4 yrs from spot
> d5   = (read "2015-12-31") :: Date -- 5 yrs from spot
> d6   = (read "2016-12-31") :: Date -- 6 yrs from spot
> d10  = (read "2020-12-31") :: Date -- 10 yrs from spot
> c0 = Cash 100.0 USD
> c1 = Cash 1000.0 USD
 
> pm1 = Payment d5 c0
> pms = [Payment d5 c0, Payment d6 c0]
> zpm = Payment d10 $ Cash 1000.0 USD
 
Present value of 100 received in 5 years, 12.0% interest annualy
> pv (df rate1) now pm1
56.742685571859916

Two payments discounted 12.0% interest rate
> map (pv (df rate1) now) pms
[56.742685571859916,50.64738419271504]

Sum of discounted cashflows
> sum $ map (pv (df rate1) now) pms
107.39006976457495

Model a Zero-Coupon Bond 
3% coupon rate, 10 years to maturity, 1000 in face
> zz = pv' (df (Flat 3)) now zpm 
743.9734067115595

Model an Annuity
5% coupon rate, 5 years to maturity, coupons of 1000 each
face of 5000, coupon of 20%
> apms = [Payment a b | a <- [d1, d2, d3, d4, d5], b <- [c1]]
> z    = sum $ map (pv' (df (Flat 5.0)) now) apms
4329.476670630818
-}
