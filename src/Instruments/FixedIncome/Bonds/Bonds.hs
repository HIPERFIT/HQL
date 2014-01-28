{-# LANGUAGE TypeFamilies, GADTs, RecordWildCards, RankNTypes, LambdaCase #-}
-- |
-- Module:      Instruments.FixedIncome.Bonds.Bonds
-- Copyright:   (c) 2013 HIPERFIT
-- License:     BSD-3
-- Maintainer:  Andreas Bock <bock@andreasbock.dk>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working with interest rates

module Instruments.FixedIncome.Bonds.Bonds where

import Control.Monad (liftM)
import qualified Data.List as L
import Utils.Calendar
import Utils.Currency
import Utils.DayCount
import Instruments.Instrument
import Instruments.Utils.TermStructure
import Instruments.Utils.InterestRate
import Prelude hiding (sum)

--
-- Types
--

type Repayment = Double
type Payment = (Date, Cash)
type Payments = [Payment]

--
-- Classes
-- 

-- | Bond class specifies common denominator for all bond types 
class Instrument b => Bond b where
  -- | Returns the present value of the bond
  pv           :: TermStructure ts => b -> ts -> IO Cash
  -- | Returns clean or dirty price on a given date
  clean, dirty :: TermStructure ts => b -> ts -> Date -> Cash
  -- | Yield to maturity
  ytm          :: TermStructure ts => b -> ts -> Double
  -- | Accrued interest
  ai           :: b -> Date -> Cash
  -- | Principal (or face value) of a bond
  principal    :: b -> Cash
  -- | A list of Payments indicating remaining cashflow
  outstanding  :: b -> Payments
  -- | Returns a list of Payments representing the cashflow
  -- over the bond over its lifetime
  cashflow     :: b -> Payments
  -- | Returns the coupon part of the cashflow
  coupons      :: b -> Payments
  -- | Returns the dates at which cashflow is exchanged
  paymentDates :: b -> [Date]
  duration     :: b -> Double
  convexity    :: b -> Payment
  
  pv bond ts = liftM (dirty bond ts) getDay
  clean bond ts now = dirty bond ts now - ai bond now
  -- If we cannot discount a given cashflow
  -- it has no theoretical value
  dirty bond ts now = sum $ zipWith discount ds cs 
    where (ds,cs) = unzip $ cashflow bond
          discount d c = case dfAt ts (diffTime d now) of
                           Just df -> scale df c
                           Nothing -> scale 0 c
  ytm = undefined
  ai = undefined

-- | Class declaration for amortized bonds
class Bond a => Amortized a where
  repayments :: a -> Payments
  repayments a = zipWith3 (\d cf cp -> (d,cf-cp)) ds cfs cps
    where (ds, cfs) = unzip $ cashflow a
          (_, cps)  = unzip $ coupons a

-- | Class for mortage-backed obligations
class Instrument m => MBO m where
  prepayment          :: m -> Cash
  periodicPrepayment  :: m -> Cash
  survivalRate        :: m -> Rate
  scheduledPrepayment :: m -> Cash
  scheduledCoupon     :: m -> Cash

--
-- Instruments
--

data FixedAmortizedBond where 
  Annuity :: { asett :: Date,
               amatu :: Date,
               aface :: Cash,
               arate :: Double,
               astms :: Settlements,
               adcc  :: (DayCount d) => d,
               aroll :: RollConvention } -> FixedAmortizedBond
  Serial :: {  asett :: Date,
               amatu :: Date,
               aface :: Cash,
               arate :: Double,
               astms :: Settlements,
               adcc  :: (DayCount d) => d,
               aroll :: RollConvention } -> FixedAmortizedBond

data FixedCouponBond where
  Zero   :: { fsett :: Date,
              fmatu :: Date,
              fface :: Cash,
              frate :: Double,
              fdcc  :: (DayCount d) => d,
              froll :: RollConvention } -> FixedCouponBond
  Consol :: { fsett :: Date,
              fface :: Cash,
              frate :: Double,
              fstms :: Settlements,
              fdcc  :: (DayCount d) => d,
              froll :: RollConvention } -> FixedCouponBond
  Bullet :: { fsett :: Date,
              fmatu :: Date,
              fface :: Cash,
              frate :: Double,
              fstms :: Settlements,
              fdcc  :: (DayCount d) => d,
              froll :: RollConvention } -> FixedCouponBond
--
-- Instances
--

instance Instrument FixedCouponBond where
  expired Zero{..} = isExpired fmatu
  expired Consol{..} = return False
  expired Bullet{..} = isExpired fmatu

instance Instrument FixedAmortizedBond where
  expired Serial{..} = isExpired amatu
  expired Annuity{..} = isExpired amatu

instance Bond FixedCouponBond where
  principal Zero{..} = fface
  principal Consol{..} = fface
  principal Bullet{..} = fface
  outstanding z@Zero{..} = [(fsett, principal z)]
  outstanding b@Bullet{..} = map (flip (,) fface) $ paymentDates b
  outstanding c@Consol{..} = map (flip (,) fface) $ paymentDates c
  coupons Zero{..} = []
  coupons Consol{..} = map (mkPayment (frate/stms) fface) dates
    where stms = fromIntegral fstms
          dates =  extrapolateDates froll fstms fsett
  coupons b@Bullet{..} = init $ cashflow b
  cashflow Zero{..} = (fmatu, fface) : []
  cashflow c@Consol{..} = coupons c
  cashflow Bullet{..}
    | dates == [] = [(fmatu,fface)]
    | otherwise   = map (\d -> (d,cpn)) (init dates) ++ [(fmatu,cpn+fface)]
    where dates = interpolateDates fmatu froll fstms fsett
          cpn = scale (frate/stms) fface
          stms  = fromIntegral fstms
  paymentDates Zero{..} = fmatu : []
  paymentDates Bullet{..} = interpolateDates fmatu froll fstms fsett
  paymentDates Consol{..} = extrapolateDates froll fstms fsett
  
  convexity = undefined
  duration  = undefined

  ytm z@Zero{..} ts = face ** (negate $ recip t) - 1
    where zpv  = pv z ts
          t    = duration z
          (Cash face _) = fface
  ytm _ _ = error "Not implemented (Newton-Raphson method)."

instance Bond FixedAmortizedBond where
  principal Serial{..} = aface
  principal Annuity{..} = aface
  outstanding s@Serial{..} = 
    let
      dates = paymentDates s
      repayment = scale (recip . fromIntegral $ length dates) aface
      outstds = take (length dates) $ iterate (\p -> p-repayment) aface
    in
      zip (asett : dates) (outstds ++ [(aface-aface)])
  outstanding Annuity{..} = undefined
  cashflow Serial{..} =
    let
      dates = interpolateDates amatu aroll astms asett
      repayment = scale (recip . fromIntegral $ length dates) aface
      accPymts :: Cash -> Date -> (Cash, Payment)
      accPymts outstd date = (outstd - repayment, (date, payment))
        where payment = repayment + scale arate outstd
    in
      snd $ L.mapAccumL accPymts aface dates
  cashflow Annuity{..} =
    let
      dates = interpolateDates amatu aroll astms asett
      yield = annualCash arate
      annualCash r = scale (recip factor) aface
        where factor = (((1+r*stms)^(floor $ getYearOffset asett amatu)) - 1)/(r*stms)
              stms = fromIntegral astms
    in
      map (flip (,) yield) dates
  coupons Serial{..} = 
    let
      dates = interpolateDates amatu aroll astms asett
      repayment = scale (recip . fromIntegral $ length dates) aface
    in
      snd $ L.mapAccumL (\o d -> (o-repayment, (d, scale arate o))) aface dates
  coupons a@Annuity{..} = zipWith3 mkCoupon ds os rp
    where (ds, os) = unzip $ outstanding a
          (_ , rp) = unzip $ repayments a
          mkCoupon d outstd rpy = (d, outstd - rpy)
  paymentDates Serial{..} = interpolateDates amatu aroll astms asett
  paymentDates Annuity{..} = interpolateDates amatu aroll astms asett
  duration = undefined
  convexity = undefined

instance Amortized FixedAmortizedBond where
  repayments Annuity{..} = snd $ L.mapAccumL accOutstd aface dates
    where sni = ((1+perPeriodRate)^n - 1)/perPeriodRate
          perPeriodRate = arate / fromIntegral astms
          n = length dates
          dates = interpolateDates amatu aroll astms asett
          accOutstd outstd date = (outstd - repayment, (date, repayment))
            where repayment = scale sni outstd

rp Annuity{..} = scale sni aface -- snd $ L.mapAccumL accOutstd aface dates
  where sni = recip $ ((1+perPeriodRate)^n - 1)/perPeriodRate
        perPeriodRate = arate / fromIntegral astms
        n = length dates
        dates = interpolateDates amatu aroll astms asett
        accOutstd outstd date = (outstd - repayment, (date, repayment))
          where repayment = scale sni outstd

mkPayment :: Rate -> Cash -> Date -> Payment
mkPayment rate face date = (date, scale rate face)

--
-- Tests
--

ts1  = AnalyticalTermStructure analyticalFun1
analyticalFun1 x = 5 + (1/2)*sqrt x

-- Tests
settle = (read "2010-01-01")::Date 
-- maturity = (read "2014-07-02")::Date
maturity = (read "2016-01-01")::Date
maturity1 = (read "2015-01-02")::Date
maturity2 = (read "2013-01-01")::Date
rate1 = 0.1
rate2 = 0.1
stms1 = 1 :: Settlements
stms2 = 2 :: Settlements
present = settle

-- Example instruments
zero    = Zero settle maturity (Cash 100 SEK) rate1 ACTACT ModifiedFollowing
bullet  = Bullet settle maturity1 (Cash 100 USD) rate1 stms2 ACTACT Following
consol  = Consol settle (Cash 100 USD) rate1 stms2 ACTACT Following
serial  = Serial settle maturity (Cash 100 USD) rate1 stms1 ACTACT Following
annuity = Annuity settle maturity2 (Cash 100 GBP) rate2 stms2 ACTACT ModifiedFollowing

-- CASHFLOW TEST
cfa = cashflow annuity

--------------------
---- PV TEST 0  ----
--------------------
tz0 = \x -> (1/15)*sqrt x
ts0 = AnalyticalTermStructure tz0
s0  = (read "2010-01-01")::Date 
m0  = (read "2017-07-01")::Date
r0  = 0.7
-- Zero PASSED
z0     = Zero s0 m0 (Cash 147 SEK) r0 ACTACT ModifiedFollowing
cf_z0  = cashflow z0
cps_z0 = coupons z0
pv_z0  = pv z0 ts0
-- Annuity
a0     = Annuity s0 m0 (Cash 100 GBP) r0 4 ACTACT ModifiedFollowing
cf_a0  = cashflow a0 -- FAILS
cps_a0 = coupons a0 -- FAILS
pv_a0  = pv a0 ts0 -- FAILS
-- Bullet
b0     = Bullet s0 m0 (Cash 100 USD) r0 4 ACTACT Following
cf_b0  = cashflow b0
cps_b0 = coupons b0
pv_b0  = pv b0 ts0 -- FAILS
-- Serial
sr0     = Serial s0 m0 (Cash 100 USD) r0 4 ACTACT Preceding
cf_sr0  = cashflow sr0 -- FAILS
cps_sr0 = coupons sr0  -- FAILS
pv_sr0  = pv sr0 ts0   -- FAILS
