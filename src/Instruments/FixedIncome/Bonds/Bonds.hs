{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
module Bonds where
import qualified Data.List as L
import Calendar
import Interest
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
              cstms :: Settlements,
              croll :: RollConvention } -> FixedCouponBond
  Bullet :: { bsett :: Date,
              bmatu :: Date,
              bface :: Cash,
              brate ::  InterestRate,
              bstms :: Settlements,
              broll :: RollConvention } -> FixedCouponBond
  Annuity :: { asett :: Date,
              amatu :: Date,
              aface :: Cash,
              arate ::  InterestRate,
              astms :: Settlements,
              aroll :: RollConvention } -> FixedCouponBond
  Serial :: { ssett :: Date,
              smatu :: Date,
              sface :: Cash,
              srate ::  InterestRate,
              sstms :: Settlements,
              sroll :: RollConvention } -> FixedCouponBond

-- instance Bond FixedCouponBond where -- TODO: Make into instance
cashflow :: FixedCouponBond -> Payments
cashflow Zero{..} = Payment zmatu zface : []
cashflow Consol{..} =
  map (mkPayment crate cface) $ extrapolateDates croll cstms csett
cashflow Bullet{..} = map (mkPayment brate bface) dates
  where dates = interpolateDates bmatu broll bstms bsett
cashflow Serial{..} = 
  let
    dates = interpolateDates smatu sroll sstms ssett
    repayment = mkRepayment dates sface
    accPymts :: Cash -> Date -> (Cash, Payment)
    accPymts outstd date = let
                             payment = repayment + scale (getRate srate) outstd
                           in
                             (outstd - repayment, Payment date payment)
  in
    snd $ L.mapAccumL accPymts sface dates
cashflow Annuity{..} =
  let
    dates = interpolateDates amatu aroll astms asett
    yield = annualCash arate
    annualCash (InterestRate (Periodic n) r) =
      let
        cmps = length dates
        factor = r/(1-(recip $ (1+r)^cmps))
      in
        scale factor aface -- TODO: Fix periodic compounding vs settlements/year
    annualCash (InterestRate Continuous r) = undefined
  in
    map (flip Payment yield) dates

mkRepayment :: [Date] -> Cash -> Cash
mkRepayment ds face = scale (recip . fromIntegral $ length ds) face

mkPayment :: InterestRate -> Cash -> Date -> Payment
mkPayment (InterestRate _ rate) face date = Payment date $ scale rate face

getComp :: InterestRate -> Compounding
getComp (InterestRate c _) = c
getRate :: InterestRate -> Double
getRate (InterestRate _ d) = d

-- Tests
settle = (read "2000-01-01")::Date 
maturity = (read "2006-01-01")::Date
rate1 = InterestRate (Periodic 1) 0.1
rate2 = InterestRate (Periodic 2) 0.1
stms1 = 1 :: Settlements
stms2 = 2 :: Settlements

serial = Serial settle maturity (Cash 100 USD) rate1 stms1 Following
annuity = Annuity settle maturity (Cash 100 GBP) rate2 stms2 ModifiedFollowing

sPayments = cashflow serial
aPayments = cashflow annuity

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
  pv :: i -> TermStructure -> IO Cash -- IO to get system time 
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
