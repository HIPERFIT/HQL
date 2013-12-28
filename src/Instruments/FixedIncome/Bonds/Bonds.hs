{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
module Bonds where
import qualified Data.List as L
import Calendar
import Currency
import TermStructure

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

data FixedCouponBond where
  Zero   :: { zsett :: Date,
              zmatu :: Date,
              zface :: Cash,
              zintr :: InterestRate,
              zroll :: RollConvention } -> FixedCouponBond
  Consol :: { csett :: Date,
              cface :: Cash,
              crate ::  InterestRate,
              croll :: RollConvention } -> FixedCouponBond
  Bullet :: { bsett :: Date,
              bmatu :: Date,
              bface :: Cash,
              brate ::  InterestRate,
              broll :: RollConvention } -> FixedCouponBond
  Annuity :: { asett :: Date,
              amatu :: Date,
              aface :: Cash,
              arate ::  InterestRate,
              aroll :: RollConvention } -> FixedCouponBond
  Serial :: { ssett :: Date,
              smatu :: Date,
              sface :: Cash,
              srate ::  InterestRate,
              sroll :: RollConvention } -> FixedCouponBond

cashflow :: FixedCouponBond -> Payments
cashflow Zero{..} = Payment zmatu zface : []
cashflow Consol{..} =
  map (mkPayment crate cface) $ extrapolateDates croll (getComp crate) csett
cashflow Bullet{..} = map (mkPayment brate bface) dates
  where dates = interpolateDates bmatu broll (getComp brate) bsett
cashflow Serial{..} = 
  let
    dates = interpolateDates smatu sroll (getComp srate) ssett
    repayment = mkRepayment dates sface
    accPymts :: Cash -> Date -> (Cash, Payment)
    accPymts outstd date = let
                             payment = repayment + scale (getRate srate) outstd
                             outstd' = outstd - repayment
                           in
                             (outstd', Payment date payment)
  in
    snd $ L.mapAccumL accPymts sface dates
cashflow Annuity{..} = undefined

mkRepayment :: [Date] -> Cash -> Cash
mkRepayment ds face = scale (recip . fromIntegral $ length ds) face

mkPayment :: InterestRate -> Cash -> Date -> Payment
mkPayment (InterestRate _ rate) face date = Payment date $ scale rate face

getComp :: InterestRate -> Compounding
getComp (InterestRate c _) = c
getRate :: InterestRate -> Double
getRate (InterestRate _ d) = d

{-
-- Tests
settle = (read "2000-01-01")::Date 
maturity = (read "2006-01-01")::Date
rate1 = InterestRate (Periodic 1) 0.1

serial = Serial settle maturity (Cash 100 USD) rate1 Following

sPayments = cashflow serial
>[Payment 2001-01-01 $26.666666666666664,Payment 2002-01-01 $25.0,
Payment 2003-01-01 $23.333333333333332,Payment 2004-01-01 $21.666666666666668,
Payment 2004-12-31 $20.0,Payment 2006-01-02 $18.333333333333332]

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
--   principal :: b -> Cash
  outstanding :: b -> Payments -- Look in DE.pdf!
--   cashflow  :: b -> Payments
  coupons   :: b -> Payments
  ytm       :: b -> Date -> Payment
--   duration  :: b -> Payment
--   clean     :: b -> Date -> Cash
--   dirty     :: b -> Date -> Cash
--   maturity  :: b -> Date

class Instrument d => Derivative d where
  underlying :: (Instrument i) => d -> i

-- class (Instrument e) => Equity e where
-- class (Instrument o) => Option o where

--
-- Instances
--

-- instance Instrument Zero where
--   pv (Zero Payment d c) = discountPayment
--   pv = undefined

-- instance Bond Zero where
--   ytm (Zero Payment d c) = undefined -- yieldAtD

{-
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
