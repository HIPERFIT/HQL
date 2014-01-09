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
--import Instruments.Utils.TermStructure
import Instruments.Utils.Discounting
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

class Instrument i where
  expired :: i -> IO Bool

-- | Bonds are instruments with the following specification (just to get Haddock up and running)
class Instrument b => Bond b where
  pv           :: b -> TermStructure -> Compounding -> IO Cash
  clean, dirty :: b -> TermStructure -> Compounding -> Date -> Cash
  ai           :: b -> Date -> Cash
  principal    :: b -> Cash
  outstanding  :: b -> Payments
  cashflow     :: b -> Payments
  coupons      :: b -> Payments
  ytm          :: b -> TermStructure -> Compounding -> Double
  paymentDates :: b -> [Date]
--   duration  :: b -> ... -> Payment
--   convexity :: b -> ... -> Payment
--   clean     :: b -> Date -> Cash
--   dirty     :: b -> Date -> Cash

-- | Class declaration for amortized bonds
class Bond r => Amortized r where
  repayments :: r -> Payments
-- class (Instrument e) => Equity e where

-- | Class for mortage-backed Obligations
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
               amat  :: Date,
               aface :: Cash,
               arate :: InterestRate,
               astms :: Settlements,
               aroll :: RollConvention } -> FixedAmortizedBond
  Serial :: {  asett :: Date,
               amatu :: Date,
               aface :: Cash,
               arate :: InterestRate,
               astms :: Settlements,
               aroll :: RollConvention } -> FixedAmortizedBond

-- TODO: Add daycount convention!
data FixedCouponBond where
  Zero   :: { fsett :: Date,
              fmatu :: Date,
              fface :: Cash,
              frate :: InterestRate,
              froll :: RollConvention } -> FixedCouponBond
  Consol :: { fsett :: Date,
              fface :: Cash,
              frate :: InterestRate,
              fstms :: Settlements,
              froll :: RollConvention } -> FixedCouponBond
  Bullet :: { fsett :: Date,
              fmatu :: Date,
              fface :: Cash,
              frate :: InterestRate,
              fstms :: Settlements,
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
  pv bond ts c = liftM (dirty bond ts c) getDay
  dirty bond ts c now = sum $ zipWith scale (discountFactors c now ts ds) cs
    where (ds, cs) = unzip $ cashflow bond
  clean bond ts c now = dirtyPrice - ai bond now
    where dirtyPrice = dirty bond ts c now
  ai = undefined

  principal Zero{..} = fface
  principal Consol{..} = fface
  principal Bullet{..} = fface

  outstanding z@Zero{..} = [(fsett, principal z)]
  outstanding b@Bullet{..} = map (flip (,) fface) $ paymentDates b
  outstanding c@Consol{..} = map (flip (,) fface) $ paymentDates c

  coupons Zero{..} = []
  coupons Consol{..} = map (mkPayment (frate/stms) fface) dates
    where stms = fromIntegral fstms
          dates =  extrapolateDates roll fstms fsett
  coupons b@Bullet{..} = case cashflow b of
                           [] -> []
                           cs -> init cs

  --
  -- Returns list of cash flows
  --
  cashflow Zero{..} = (fmatu, fface) : []
  cashflow c@Consol{..} = coupons c
  cashflow Bullet{..} = map (mkPayment (frate/stms) fface) dates
    where dates = interpolateDates matu roll fstms fsett
          stms  = fromIntegral fstms

  paymentDates Zero{..} = fmatu : []
  paymentDates Bullet{..} = interpolateDates fmatu froll fstms fsett
  paymentDates Consol{..} = extrapolateDates froll fstms fsett

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
      accPymts outstd date = let
                               payment = repayment + scale arate outstd
                             in
                               (outstd - repayment, (date, payment))
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

  --
  -- Compute coupons for a bond 
  --
  coupons Serial{..} = 
    let
      dates = interpolateDates amatu aroll astms asett
      repayment = scale (recip . fromIntegral $ length dates) aface
    in
      snd $ L.mapAccumL (\o d -> (o-repayment, (d, scale arate o))) aface dates
  coupons Annuity{..} = undefined
  ytm z@Zero{..} ts cmp = face ** (negate $ recip t) - 1
    where zpv  = pv z ts cmp
          t    = getYearOffset asett amatu
          (Cash face _) = aface

  paymentDates Serial{..} = interpolateDates smatu sroll sstms ssett
  paymentDates Annuity{..} = interpolateDates amatu aroll astms asett

mkPayment :: InterestRate -> Cash -> Date -> Payment
mkPayment rate face date = (date, scale rate face)

-- The following classes and types are purely proof of concept
type FloatingCouponBond = Int
data Option where 
  Call :: { callint :: Int } -> Option
  Put  :: { putint :: Int } -> Option
  Swap :: { fxcb :: FixedCouponBond,
            flcb :: FloatingCouponBond } -> Option

class Instrument d => Derivative d where
  data Underlying :: *
  underlying :: d -> Underlying

instance Instrument Option where
  expired = undefined

instance Derivative Option where
  data Underlying = Legs (FixedCouponBond, FloatingCouponBond)
                  | Commodity Int
  underlying Call{..} = Commodity callint
  underlying Put{..}  = Commodity putint
  underlying Swap{..}  = Legs (fxcb,flcb)

ts1  = Analytical analyticalFun1
analyticalFun1 x = 5 + (1/2)*sqrt x

-- Tests
settle = (read "2010-01-01")::Date 
maturity = (read "2014-07-02")::Date
-- maturity = (read "2016-01-01")::Date
maturity1 = (read "2015-01-02")::Date
maturity2 = (read "2013-01-01")::Date
rate1 = 0.1
rate2 = 0.1
stms1 = 1 :: Settlements
stms2 = 2 :: Settlements
present = settle

-- Example instruments
zero    = Zero settle maturity (Cash 100 SEK) rate1 Preceding
bullet  = Bullet settle maturity1 (Cash 100 USD) rate1 stms2 Following
consol  = Consol settle (Cash 100 USD) rate1 stms2 Following
serial  = Serial settle maturity (Cash 100 USD) rate1 stms1 Following
annuity = Annuity settle maturity2 (Cash 100 GBP) rate2 stms2 ModifiedFollowing

-- Actual tests
-- analyticalFun1 (getYearOffset settle maturity) == 6.0608215993999295
-- discountFactor' (analyticalFun1 (getYearOffset settle maturity)) (getYearOffset settle maturity) 0 == 0.7612297990008563
-- discountFactor' 6.0608215993999295 (getYearOffset settle maturity) 0 -- 0.7612297990008563
test0 = [(dirty zero ts1 Continuous present) == (Cash 70.56481405950817 SEK)] -- fails, Discounting.hs bug
tests = [test0]
