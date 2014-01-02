{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
module Bonds where
import qualified Data.List as L
import Calendar
import Currency
import TermStructure
import Discounting

--
-- Types
--

type Repayment = Double
type Payment = (Date, Cash)
-- instance Show Payment where
--     show (Payment (d,c)) = "{" ++ show d ++ ", " ++ show c ++ "}"
type Payments = [Payment]
type Cashflow = Payments

--
-- Classes
-- 

class Instrument i where
  expired :: i -> IO Bool
  yrsToExpiry :: i -> Date -> Years

class Instrument b => Bond b where
  pv :: b -> TermStructure -> Compounding -> IO Cash
  fv :: b -> TermStructure -> Compounding -> Date -> Cash
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
--   maturity  :: b -> Date

class Instrument d => Derivative d where
  underlying :: (Instrument i) => d -> i

-- class (Instrument e) => Equity e where
-- class (Instrument o) => Option o where

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
  yrsToExpiry _ = undefined -- Refactor to use this in `expired`

instance Bond FixedCouponBond where
  pv bond ts c = getDay >>= return . fv bond ts c
  fv bond ts c now = sum $ zipWith scale (discountFactors c now ts ds) cs
    where (ds, cs) = unzip $ cashflow bond

  principal Zero{..} = zface
  principal Consol{..} = cface
  principal Bullet{..} = bface
  principal Serial{..} = sface
  principal Annuity{..} = aface

  outstanding z@Zero{..} = [(zmatu, principal z)]
  outstanding b@Bullet{..} = map (flip (,) bface) $ paymentDates b
  outstanding c@Consol{..} = map (flip (,) cface) $ paymentDates c
  outstanding s@Serial{..} = 
    let
      dates = paymentDates s
      repayment = scale (recip . fromIntegral $ length dates) sface
      outstds = take (length dates) $ iterate (\p -> p-repayment) sface
    in
      zipWith ((,)) dates outstds
  outstanding Annuity{..} = undefined

  cashflow Zero{..} = (zmatu, zface) : []
  cashflow Consol{..} = map (mkPayment crate cface) $ extrapolateDates croll cstms csett
  cashflow Bullet{..} = map (mkPayment brate bface) dates
    where dates = interpolateDates bmatu broll bstms bsett
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
      annualCash r =
        let
          cmps = length dates
          factor = r/(1-(recip $ (1+r)^cmps))
        in
          scale factor aface -- TODO: Fix periodic compounding vs settlements/year
    in
      map (flip (,) yield) dates
  coupons = undefined
  ytm = undefined

  paymentDates Zero{..} = zmatu : []
  paymentDates Bullet{..} = interpolateDates bmatu broll bstms bsett
  paymentDates Consol{..} = extrapolateDates croll cstms csett
  paymentDates Serial{..} = interpolateDates smatu sroll sstms ssett
  paymentDates Annuity{..} = interpolateDates amatu aroll astms asett
  
mkPayment :: InterestRate -> Cash -> Date -> Payment
mkPayment rate face date = (date, scale rate face)

-- Tests
settle = (read "2000-01-01")::Date 
maturity = (read "2006-01-01")::Date
rate1 = 0.1
rate2 = 0.1
stms1 = 1 :: Settlements
stms2 = 2 :: Settlements

serial = Serial settle maturity (Cash 100 USD) rate1 stms1 Following
annuity = Annuity settle maturity (Cash 100 GBP) rate2 stms2 ModifiedFollowing

sPayments = cashflow serial
aPayments = cashflow annuity
