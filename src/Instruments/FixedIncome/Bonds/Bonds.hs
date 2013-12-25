{-# LANGUAGE RecordWildCards #-}
module Bonds where
import qualified Data.Map as M
import qualified Data.List as L
import Calendar
import Currency
import TermStructure
import Data.Char

--
-- Types
--

type Repayment = Double
data Payment = Payment Date Cash deriving (Show)
type Payments = [Payment]
type Cashflow = Payments

--
-- Instruments
--

-- Currently only supporting fixed coupon bonds
newtype Zero   = Zero { payment :: Payment }
newtype Consol = Consol { cpayments :: Payments }
newtype Bullet = Bullet { bpayments :: Payments }
newtype Annuity = Annuity { apayments :: Payments }
newtype Serial = Serial { spayments :: Payments }


-- This is the input data, will be converted to a portfolio of
-- zero coupon bonds (discount factors)
data FixedCouponBond = FixedCouponBond {
  settlement :: Date,
  maturity   :: Date,
  couponRate :: InterestRate,
  roll       :: RollConvention,
  faceValue  :: Double,
  rate       :: InterestRate,
  currency   :: Currency
}

zero :: Date -> Cash -> Zero
zero d c = Zero $ Payment d c

{-
consol :: FixedCouponBond -> Currency -> Consol
consol FixedCouponBond{..} =
  let
    mkPayment date = Payment date $ Cash (faceValue * couponRate) currency
    dates = getSettlementDates roll period settlement
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

-}

{-
    **** Notes
    
    Every bond will produce a series of payments, depending on settings

    DiscountFunction will be depending on the period

    TermStructure will determine the interest rate at each point in time

    ** Easy to extend to use dates etc, and amortize for passed interest coupons

    ** Able to use PV, FV and Value(t) using this method
-}

--
-- Classes
-- 

class Instrument i where
  pv :: i -> TermStructure -> Date -> Cash
  expired  :: i -> Date -> Bool
  yrsToExpiry :: i -> Date -> Years

class Instrument b => Bond b where
  principal :: b -> Cash
  cashflow  :: b -> Payments
  coupon    :: b -> Cash
  ytm       :: b -> Date -> Payment
  duration  :: b -> Payment
  clean     :: b -> Date -> Cash
  dirty     :: b -> Date -> Cash
--   maturity  :: b -> Date

class Instrument d => Derivative d where
  underlying :: (Instrument i) => d -> i

-- class (Instrument e) => Equity e where
-- class (Instrument o) => Option o where

--
-- Instances
--

{-
instance Instrument Zero where
instance Instrument Consol where
instance Instrument Bullet where
instance Instrument Annuity where
instance Instrument Serial where

instance Bond Zero where
instance Bond Consol where
instance Bond Bullet where
instance Bond Annuity where
instance Bond Serial where
-}
