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

type Repayment = Double

data Basis = ACTACT | ACT360 | ACT365F | Thirty360 | SIA | Business | European | Japanese

data Payment = Payment Date Cash deriving (Show)
type Payments = [Payment]
type Cashflow = Payments

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
data EndMonthRule = Ignore | Apply

data BaseBond = BaseBond {
  settle       :: Date,           -- Settlement date
  currency     :: Currency,       -- Currency
  basematurity :: Date,           -- Maturity date
  couponRate   :: InterestRate,   -- Coupon rate
  period       :: Int,            -- Settlements per year
  basis        :: Basis,          -- Day-count basis
  roll         :: RollConvention, -- How to roll days
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

-- From Bonds2.hs:

type IssueDate = Date
type CouponRates = [Double]

data Bond' = Bond' {
    issueDate'    :: IssueDate,
    settlements'  :: Settlements,   -- Settlement date
    maturity'     :: Date,          -- Maturity date
    couponRates'  :: CouponRates,   -- Coupon rate
    compounding'  :: Compounding,           -- Coupons per year (default = 2)
    basis'        :: Basis,         -- Day-count basis
    endMonthRule' :: EndMonthRule,  -- End-of-month rule
    face         :: Double         -- Face value of bond
}

{-
    **** Notes
    
    Every bond will produce a series of payments, depending on settings
    
    DiscountFunction will be depending on the period
    
    TermStructure will determine the interest rate at each point in time
    
    ** Easy to extend to use dates etc, and amortize for passed interest coupons
    
    ** Able to use PV, FV and Value(t) using this method

-}

--
-- Instances
--
{-

instance Instrument Zero where
  pv z@(Zero (Payment d c)) r p = flip scale c $ recip ((1+r) ** yrsToExpiry z p)
  expired (Zero (Payment d c)) p = 0 >= T.diffDays p d
  cashflow (Zero payment) = [payment]
  yrsToExpiry (Zero (Payment d c)) p = fromInteger (T.diffDays d p) / 365 -- TODO: 365 is not always right, right?

instance Instrument Consol where
  pv (Consol (Payment _ c:_)) r = const $ scale (recip r) c
  expired _ _ = False
  yrsToExpiry _ _ = undefined
  cashflow (Consol ps) = ps

instance Instrument Bullet where
  pv (Bullet ps@(Payment _ (Cash c _):_)) r p = Cash c $ sum $ map (discountPayment r p) ps
  expired b p = 0 >= (T.diffDays p $ maturity b)
  yrsToExpiry b p = fromInteger (T.diffDays (maturity b) p) / 365
  cashflow (Bullet ps) = ps

instance Instrument Annuity where
  pv (Annuity ps@(Payment _ (Cash c _):_)) r p = Cash c $ sum $ map (discountPayment r p) ps
  expired b p = 0 >= (T.diffDays p $ maturity b)
  yrsToExpiry b p = fromInteger (T.diffDays (maturity b) p) / 365
  cashflow (Annuity ps) = ps

instance Bond Bullet where
  principal (Bullet ps) = let (Payment d c) = last ps in c
  coupon (Bullet (Payment d c:ps)) = c
  maturity (Bullet ps) = let (Payment d c) = last ps in d
  ytm (Bullet _) p = undefined
  duration z = undefined

instance Bond Zero where
  principal (Zero (Payment d c)) = c
  coupon (Zero (Payment d (Cash c v))) = Cash c 0
  maturity (Zero (Payment d _)) = d
  ytm z@(Zero (Payment _ _)) p = undefined
  duration z = undefined

instance Bond Consol where
  principal (Consol (Payment d c:ps)) = undefined -- What do we do here..?
  coupon (Consol (Payment _ c:_)) = c
  maturity _ = undefined -- what is the biggest UTCTime we have? :P
  ytm c = undefined
  duration b = undefined

instance Bond Annuity where
  principal (Annuity (Payment d c:ps)) = undefined -- What do we do here..?
  coupon (Annuity (Payment _ c:_)) = c
  maturity _ = undefined -- what is the biggest UTCTime we have? :P
  ytm c = undefined
  duration b = undefined

-- instance Bond SimpleAnnuity where
-- instance Bond MortgageBackedBond where
-}
