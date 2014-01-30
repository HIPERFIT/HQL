{-# LANGUAGE TypeFamilies, GADTs, RecordWildCards, RankNTypes #-}
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
  clean, dirty :: b -> TermStructure -> Date -> Cash
  -- | Yield to maturity
  ytm          :: b -> TermStructure -> Double
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
  
  clean bond ts now = dirty bond ts now - ai bond now
  -- If we cannot discount a given cashflow
  -- it has no theoretical value
  dirty bond ts now = sum $ zipWith discount dfs cs
    where (ds,cs) = unzip $ cashflow bond
          dfs = dfsAt ts $ map (getYearOffset now) ds
          discount (Just df) = scale df
          discount Nothing   = scale 0 
  ytm = undefined
  ai = undefined

-- | Class declaration for amortized bonds
class Bond a => Amortized a where
  repayments :: a -> Payments
  repayments a = zip ds $ zipWith (-) cf cps
    where (ds,cf) = unzip $ cashflow a
          cps = snd $ unzip $ coupons a

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

instance Instrument FixedCouponBond where
  type PricingEngine = TermStructure
  pv c@Consol{..} _ = return $ scale frate fface
  pv bond ts = liftM (dirty bond ts) getDay
  expired Zero{..} = isExpired fmatu
  expired Consol{..} = return False
  expired Bullet{..} = isExpired fmatu

instance Instrument FixedAmortizedBond where
  type PricingEngine = TermStructure
  pv bond ts = liftM (dirty bond ts) getDay
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
  cashflow Annuity{..} = map (flip (,) yield) dates
    where dates = interpolateDates amatu aroll astms asett
          yield = scale (recip $ (1-(1+r)**(-n))/r) aface
          n = fromIntegral $ length dates - 1 -- Subtract settlement
          r = arate / fromIntegral astms
  coupons Serial{..} = 
    let
      dates = interpolateDates amatu aroll astms asett
      repayment = scale (recip . fromIntegral $ length dates) aface
    in
      snd $ L.mapAccumL (\o d -> (o-repayment, (d, scale arate o))) aface dates
  coupons a@Annuity{..} = zip (tail ds) $ snd $ L.mapAccumL mkCpns aface $ tail cfs
    where (ds, cfs) = unzip $ cashflow a
          repayment = scale (recip $ (1-(1+perPeriodRate)**(-n))/perPeriodRate) aface
          mkCpns outstd cf = (outstd - repayment, coupon)
            where coupon = scale perPeriodRate outstd
          n = fromIntegral $ length ds
          perPeriodRate = arate / fromIntegral astms

  paymentDates Serial{..} = interpolateDates amatu aroll astms asett
  paymentDates Annuity{..} = interpolateDates amatu aroll astms asett
  duration = undefined
  convexity = undefined

instance Amortized FixedAmortizedBond

mkPayment :: Rate -> Cash -> Date -> Payment
mkPayment rate face date = (date, scale rate face)

--
-- Tests
--
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

---------------
---- TESTS ----
---------------
tz0 = \x -> (5 + (1/4)*sqrt x)/100
ts0 = AnalyticalTermStructure tz0
s0  = (read "2015-01-01")::Date 
m0  = (read "2022-07-01")::Date
r0  = 0.07
stms0 = 2
-- Zero
z0     = Zero s0 m0 (Cash 147 SEK) r0 ACTACT ModifiedFollowing
cf_z0  = cashflow z0
cps_z0 = coupons z0
pv_z0  = pv z0 ts0
-- Annuity
a0     = Annuity s0 m0 (Cash 100 GBP) r0 stms0 ACTACT ModifiedFollowing
cf_a0  = cashflow a0
cps_a0 = coupons a0
pv_a0  = pv a0 ts0   -- FAILS
-- Bullet
b0     = Bullet s0 m0 (Cash 100 USD) r0 stms0 ACTACT Following
cf_b0  = cashflow b0
cps_b0 = coupons b0
pv_b0  = pv b0 ts0
-- Consol
c0     = Consol s0 (Cash 100 USD) r0 stms0 ACTACT Following
cf_c0  = cashflow c0
cps_c0 = coupons c0
pv_c0  = pv c0 ts0
-- Serial
sr0     = Serial s0 m0 (Cash 100 USD) r0 stms0 ACTACT Preceding
cf_sr0  = cashflow sr0
cps_sr0 = coupons sr0
pv_sr0  = pv sr0 ts0 -- FAILS
