{-# LANGUAGE DisambiguateRecordFields #-}
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
type DiscountFunction = Date -> Payment -> Payment

--
-- Instruments
--

newtype Vanilla = Vanilla { payments :: Payments }
newtype Zero   = Zero { payment :: Payment } deriving (Show)
newtype Consol = Consol { cpayments :: Payments }
newtype Bullet = Bullet { bpayments :: Payments }
newtype Annuity = Annuity { apayments :: Payments }
newtype Serial = Serial { spayments :: Payments }

data BaseBond = BaseBond {
    settle       :: Date,           -- Settlement date
    currency     :: Currency,       -- Currency
    basematurity :: Date,           -- Maturity date
    couponRate   :: InterestRate,   -- Coupon rate
    period       :: Int,            -- Settlements per year
    basis        :: Basis,          -- Day-count basis
    endMontRule  :: RollConvention, -- End-of-month rule
    faceValue    :: Double          -- Face value of bond
}

zero :: BaseBond -> Zero
zero (BaseBond s c m i p b e fv) = Zero $ Payment m $ Cash c fv

-- Add record syntax for pattern-match (order matters)
consol :: BaseBond -> Consol
consol (BaseBond s c m i p b e fv) = -- BaseBond{..}
  let
    mkPayment date = Payment date $ Cash c $ fv * i
    dates = getSettlementDates e p s -- endof month
  in
    Consol $ map mkPayment dates

bullet :: BaseBond -> Bullet
bullet (BaseBond s c m i p b e fv) =
  let
    mkPayment date = Payment date $ Cash c $ fv * i
    dates = getSettlementDates e p s
  in
    Bullet $ map mkPayment dates

-- Serial takes a BaseBond as well as a fixed repayment
serial :: Repayment -> BaseBond -> Serial
serial repayment (BaseBond s c m i p b e fv) =
  let
    dates = getSettlementDates e p s
    fv' = Cash c fv
    fun (Cash c outstd) date = let
                                 coupon  = i * outstd
                                 outstd' = outstd - (coupon + repayment)
                               in (Cash c outstd', Payment date $ Cash c coupon)
    (remaining, payments) = L.mapAccumL fun fv' dates
  in
    Serial $ payments ++ [Payment m remaining] 

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

class Instrument d => Derivative d where
  underlying :: (Instrument i) => d -> i

-- class (Instrument e) => Equity e where
-- class (Instrument o) => Option o where


