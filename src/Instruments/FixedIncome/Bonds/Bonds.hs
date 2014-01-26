{-# LANGUAGE TypeFamilies, GADTs, RecordWildCards, RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
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

-- | Instrument is a high-level class for financial instruments
class Instrument i where
  expired :: i -> IO Bool

-- | Bond class specifies common denominator for all bond types 
class Instrument b => Bond b where
  pv           :: TermStructure ts => b -> ts -> IO Cash
  clean, dirty :: TermStructure ts => b -> ts -> Date -> Cash
  ytm          :: TermStructure ts => b -> ts -> Double
  ai           :: b -> Date -> Cash
  principal    :: b -> Cash
  outstanding  :: b -> Payments
  cashflow     :: b -> Payments
  coupons      :: b -> Payments
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
  cashflow Bullet{..} = map (mkPayment (frate/stms) fface) dates
    where dates = interpolateDates fmatu froll fstms fsett
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
        where factor = (((1+r)^(length dates)) - 1)/r
    in
      map (flip (,) yield) dates
  coupons Serial{..} = 
    let
      dates = interpolateDates amatu aroll astms asett
      repayment = scale (recip . fromIntegral $ length dates) aface
    in
      snd $ L.mapAccumL (\o d -> (o-repayment, (d, scale arate o))) aface dates
  coupons Annuity{..} = undefined
  paymentDates Serial{..} = interpolateDates amatu aroll astms asett
  paymentDates Annuity{..} = interpolateDates amatu aroll astms asett
  duration = undefined
  convexity = undefined

instance Amortized FixedAmortizedBond where

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
zero    = Zero settle maturity (Cash 100 SEK) rate1 Preceding ModifiedFollowing
bullet  = Bullet settle maturity1 (Cash 100 USD) rate1 stms2 Following
consol  = Consol settle (Cash 100 USD) rate1 stms2 Following
serial  = Serial settle maturity (Cash 100 USD) rate1 stms1 Following
annuity = Annuity settle maturity2 (Cash 100 GBP) rate2 stms2 ACTACT ModifiedFollowing

annuity' = Annuity settle maturity2 (Cash 100 GBP) rate2 stms2 ModifiedFollowing
-- Actual tests
-- analyticalFun1 (getYearOffset settle maturity) == 6.0608215993999295
-- discountFactor' (analyticalFun1 (getYearOffset settle maturity)) (getYearOffset settle maturity) 0 == 0.7612297990008563
-- discountFactor' 6.0608215993999295 (getYearOffset settle maturity) 0 -- 0.7612297990008563
-- test0 = [(dirty zero ts1 Continuous present) == (Cash 70.56481405950817 SEK)] -- fails, Discounting.hs bug
-- tests = [test0]
