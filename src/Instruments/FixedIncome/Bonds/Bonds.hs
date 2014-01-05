{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs, RecordWildCards #-}
module Instruments.FixedIncome.Bonds.Bonds where

import Control.Monad (liftM)
import qualified Data.List as L
import Utils.Calendar
import Utils.Currency
import Instruments.Utils.TermStructure
import Instruments.Utils.Discounting
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

class Instrument b => Bond b where
  pv :: b -> TermStructure -> Compounding -> IO Cash
  clean, dirty :: b -> TermStructure -> Compounding -> Date -> Cash
  ai :: b -> Date -> Cash
  principal :: b -> Cash
  outstanding :: b -> Payments
  cashflow  :: b -> Payments
  coupons   :: b -> Payments
  ytm       :: b -> Date -> Payment
  paymentDates :: b -> [Date]
--   duration  :: b -> ... -> Payment
--   convexity :: b -> ... -> Payment
--   clean     :: b -> Date -> Cash
--   dirty     :: b -> Date -> Cash

-- Repayable class for Serials, Annuities, MBBs
class Bond r => Repayable r where
  repayment :: r -> [Cash]
-- class (Instrument e) => Equity e where

--
-- Instruments
--

-- TODO: Add daycount convention!
data FixedCouponBond where
  Zero   :: { zsett :: Date,
              zmatu :: Date,
              zface :: Cash,
              zintr :: InterestRate,
              zroll :: RollConvention } -> FixedCouponBond
  Consol :: { csett :: Date,
              cface :: Cash,
              crate :: InterestRate,
              cstms :: Settlements,
              croll :: RollConvention } -> FixedCouponBond
  Bullet :: { bsett :: Date,
              bmatu :: Date,
              bface :: Cash,
              brate :: InterestRate,
              bstms :: Settlements,
              broll :: RollConvention } -> FixedCouponBond
  Annuity :: { asett :: Date,
              amatu :: Date,
              aface :: Cash,
              arate :: InterestRate,
              astms :: Settlements,
              aroll :: RollConvention } -> FixedCouponBond
  Serial :: { ssett :: Date,
              smatu :: Date,
              sface :: Cash,
              srate :: InterestRate,
              sstms :: Settlements,
              sroll :: RollConvention } -> FixedCouponBond

--
-- Instances
--

instance Instrument FixedCouponBond where
  expired Zero{..} = isExpired zmatu
  expired Consol{..} = return False
  expired Bullet{..} = isExpired bmatu
  expired Serial{..} = isExpired smatu
  expired Annuity{..} = isExpired amatu

instance Bond FixedCouponBond where
  pv bond ts c = liftM (dirty bond ts c) getDay
  dirty bond ts c now = sum $ zipWith scale (discountFactors c now ts ds) cs
    where (ds, cs) = unzip $ cashflow bond
  clean bond ts c now = dirtyPrice - ai bond now
    where dirtyPrice = dirty bond ts c now
  ai = undefined

  principal Zero{..} = zface
  principal Consol{..} = cface
  principal Bullet{..} = bface
  principal Serial{..} = sface
  principal Annuity{..} = aface

  outstanding z@Zero{..} = [(zsett, principal z)]
  outstanding b@Bullet{..} = map (flip (,) bface) $ paymentDates b
  outstanding c@Consol{..} = map (flip (,) cface) $ paymentDates c
  outstanding s@Serial{..} = 
    let
      dates = paymentDates s
      repayment = scale (recip . fromIntegral $ length dates) sface
      outstds = take (length dates) $ iterate (\p -> p-repayment) sface
    in
      zip (ssett : dates) (outstds ++ [(sface-sface)])
  outstanding Annuity{..} = undefined

  --
  -- Returns list of cash flows
  --
  cashflow Zero{..} = (zmatu, zface) : []
  cashflow c@Consol{..} = coupons c
  cashflow Bullet{..} = map (mkPayment (brate/stms) bface) dates
    where dates = interpolateDates bmatu broll bstms bsett
          stms  = fromIntegral bstms
  cashflow Serial{..} = 
      let
      dates = interpolateDates smatu sroll sstms ssett
      repayment = scale (recip . fromIntegral $ length dates) sface
      accPymts :: Cash -> Date -> (Cash, Payment)
      accPymts outstd date = let
                               payment = repayment + scale srate outstd
                             in
                               (outstd - repayment, (date, payment))
    in
      snd $ L.mapAccumL accPymts sface dates
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
  coupons Zero{..} = []
  coupons Consol{..} = map (mkPayment (crate/stms) cface) dates
    where stms = fromIntegral cstms
          dates =  extrapolateDates croll cstms csett
  coupons b@Bullet{..} = case cashflow b of
                           [] -> []
                           cs -> init cs
  coupons Serial{..} = 
    let
      dates = interpolateDates smatu sroll sstms ssett
      repayment = scale (recip . fromIntegral $ length dates) sface
    in
      snd $ L.mapAccumL (\o d -> (o-repayment, (d, scale srate o))) sface dates
  coupons Annuity{..} = undefined
  ytm = undefined

  paymentDates Zero{..} = zmatu : []
  paymentDates Bullet{..} = interpolateDates bmatu broll bstms bsett
  paymentDates Consol{..} = extrapolateDates croll cstms csett
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
test0 = [(dirty zero ts1 Continuous present) == (Cash 70.56481405950817 SEK)] -- fails, Discounting.hs bug
tests = [test0]
