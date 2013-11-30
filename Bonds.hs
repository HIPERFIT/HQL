-- {-# LANGUAGE GADTs, RankNTypes #-}
module Bonds where
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Time as T
import Currency
import TermStructure
import Control.Monad
import Prelude hiding (exp)

--
-- Types
--

data Basis = ACTACT | ACT360 | ACT365F | Thirty360 | SIA | Business | European | Japanese
data RollConvention = Following | Preceding | ModifiedFollowing

type Days = Integer
type InterestRate = Double
type Years = Double -- Can be fractional

data Payment = Payment Date Cash
type Payments = [Payment]
type Settlements = Int
type DiscountFactor = Double

type DiscountFunction = InterestRate -> Days -> DiscountFactor
-- Could be changed to:
-- type DiscountFunction = Day -> TermStructure -> InterestRate
-- if we change some things (this means we might need to build curves..)

--
-- Instruments
--

newtype Vanilla = Vanilla { payments :: Payments }
newtype Zero   = Zero { payment :: Payment }
newtype Consol = Consol { cpayments :: Payments }
newtype Bullet = Bullet { bpayments :: Payments }
newtype Annuity = Annuity { apayments :: Payments }

data Bond = Bond {
    settle      :: Date,           -- Settlement date
    currency    :: Currency,       -- Currency
    maturity    :: Date,           -- Maturity date
    couponRate  :: InterestRate,   -- Coupon rate
    period      :: Int,            -- Settlements per year
    basis       :: Basis,          -- Day-count basis
    endMontRule :: RollConvention, -- End-of-month rule
    faceValue   :: Double          -- Face value of bond
}

zero :: Bond -> Zero
zero (Bond s c m i p b e fv) = Zero $ Payment m $ Cash c fv

consol :: Bond -> Consol
consol (Bond s c m i p b e fv) =
  let
    mkPayment date = Payment date $ Cash c $ fv * i
    dates = getSettlementDates e b p s
  in
    Consol $ map mkPayment dates

bullet :: Bond -> Bullet
bullet (Bond s c m i p b e fv) =
  let
    mkPayment date = Payment date $ Cash c $ fv * i
    dates = getSettlementDates e b p s
  in
    Bullet $ map mkPayment dates

type Repayment = Double

-- Serial takes a Bond as well as a fixed repayment
serial :: Repayment -> Bond -> Annuity
serial repayment (Bond s c m i p b e fv) =
  let
    dates = getSettlementDates e b p s
    fv' = Cash c fv
    fun = \(Cash c outstd) date -> let
                                     coupon  = i * outstd
                                     outstd' = outstd - (coupon + repayment)
                                   in (Cash c outstd', Payment date $ Cash c coupon)
    (remaining, payments) = L.mapAccumL fun fv' dates
  in
    Annuity $ payments ++ [Payment m remaining] 

df :: DiscountFunction
df r n = 1.0 / (1.0 + (r / 100.0)) ** (fromIntegral n)

-- type Present = Date
-- pv :: Present -> DiscountFunction -> Payment -> Payment
-- pv p df p@(Payment date cash) = (df y) * a
--   where 

yrsToExpiry :: Date -> Date -> Integer
yrsToExpiry d p = floor $ fromInteger (T.diffDays p d) / 365

getSettlementDates :: RollConvention -> Basis -> Settlements -> Date -> [Date]
getSettlementDates r b sts dt = map (rollDay r) $ iterate nextDate dt
 where nextDate date = T.addDays daysBetween date
       daysBetween   = daysBetweenSettlements dt sts

daysBetweenSettlements :: Date -> Settlements -> Days
daysBetweenSettlements d sts 
  | leapYear d = floor $ 366.0 / fromIntegral sts
  | otherwise  = floor $ 365.0 / fromIntegral sts

rollDay :: RollConvention -> Date -> Date
rollDay = undefined

leapYear :: Date -> Bool
leapYear = undefined

legalDay :: Date -> Bool
legalDay = undefined

--
-- Classes
-- 

{-
class Instrument i where
  pv       :: i -> Date -> Payment
  expired  :: i -> Date -> Bool
  yrsToExpiry :: i -> Date -> Double
  cashflow :: i -> Payments

class Instrument b => Bond b where
  principal :: b -> Payment
  coupon    :: b -> Payment
  maturity  :: b -> Date
  ytm       :: b -> Date -> Payment
  duration  :: b -> Payment
  discount  :: b -> DiscountFunction

class Instrument d => Derivative d where
  underlying :: (Instrument i) => d -> i

-- class (Instrument e) => Equity e where
-- class (Instrument o) => Option o where

--
-- Instances
--

instance Instrument Zero where
  pv z@(Zero (Vanilla f d r)) p = scale f $ recip ((1+r)** yrsToExpiry z p)
  expired z@(Zero (Vanilla f d r)) p = 0 >= T.diffDays p d
  yrsToExpiry z@(Zero (Vanilla f d r)) p = fromIntegral (T.diffDays p d)/365::Double
  cashflow z@(Zero (Vanilla f d r)) = M.insert d f M.empty

-- instance Instrument Consol where
--   pv p (Consol r c) = scale c r
--   expired _ _ = False
--   cashflow (Consol ts c) = undefined

instance Bond Zero where
  principal (Zero (Vanilla f _ _)) = f
  coupon (Zero (Vanilla f d r)) = scale f r
  maturity (Zero (Vanilla _ d _)) = d
  ytm z@(Zero (Vanilla f d r)) p = add (-1) $ exp (f/pv z p) e
    where e = recip $ yrsToExpiry z p
  duration z = undefined
  discount z = undefined

instance Bond Consol where
  principal (Consol ts coupon) = undefined
  coupon (Consol _ c) = c
  maturity _ = undefined -- what is the biggest UTCTime we have? :P
  ytm c = undefined
  duration b = undefined
  discount b = undefined
-}

-- instance Bond Annuity where
-- instance Bond SimpleAnnuity where
-- instance Bond MortgageBackedBond where
