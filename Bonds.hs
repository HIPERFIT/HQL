module Bonds where
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Time as T
import qualified Data.Time.Calendar.WeekDate as WeekDate
import qualified Data.Time.Calendar as Cal
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
type Repayment = Double

data Payment = Payment Date Cash deriving (Show)
type Payments = [Payment]
type Settlements = Int
type DiscountFunction = Date -> Payment -> Payment

-- type DiscountFunction = InterestRate -> Days -> DiscountFactor
-- Could be changed to:
-- type DiscountFunction = Day -> TermStructure -> InterestRate
-- if we change some things (this means we might need to build curves..)

--
-- Instruments
--

newtype Vanilla = Vanilla { payments :: Payments }
newtype Zero   = Zero { payment :: Payment } deriving (Show)
newtype Consol = Consol { cpayments :: Payments }
newtype Bullet = Bullet { bpayments :: Payments }
newtype Annuity = Annuity { apayments :: Payments }
newtype Serial = Serial { spayments :: Payments }

data BaseBond = BaseBond {
    settle       :: Date,           -- Settlement date
    currency     :: Currency,       -- Currency
    basematurity :: Date,           -- Maturity date
    couponRate   :: InterestRate,   -- Coupon rate
    period       :: Int,            -- Settlements per year
    basis        :: Basis,          -- Day-count basis
    endMontRule  :: RollConvention, -- End-of-month rule
    faceValue    :: Double          -- Face value of bond
}

zero :: BaseBond -> Zero
zero (BaseBond s c m i p b e fv) = Zero $ Payment m $ Cash c fv

consol :: BaseBond -> Consol
consol (BaseBond s c m i p b e fv) =
  let
    mkPayment date = Payment date $ Cash c $ fv * i
    dates = getSettlementDates e b p s
  in
    Consol $ map mkPayment dates

bullet :: BaseBond -> Bullet
bullet (BaseBond s c m i p b e fv) =
  let
    mkPayment date = Payment date $ Cash c $ fv * i
    dates = getSettlementDates e b p s
  in
    Bullet $ map mkPayment dates

-- Serial takes a BaseBond as well as a fixed repayment
serial :: Repayment -> BaseBond -> Serial
serial repayment (BaseBond s c m i p b e fv) =
  let
    dates = getSettlementDates e b p s
    fv' = Cash c fv
    fun (Cash c outstd) date = let
                                 coupon  = i * outstd
                                 outstd' = outstd - (coupon + repayment)
                               in (Cash c outstd', Payment date $ Cash c coupon)
    (remaining, payments) = L.mapAccumL fun fv' dates
  in
    Serial $ payments ++ [Payment m remaining] 

-- df :: DiscountFunction
-- df r n = 1.0 / (1.0 + (r / 100.0)) ** (fromIntegral n)
-- type Present = Date
-- pv :: Present -> DiscountFunction -> Payment -> Payment
-- pv p df p@(Payment date cash) = (df y) * a
--   where 


-- TODO: What about the Basis? Only for discounting? if so, add to bond newtypes
getSettlementDates :: RollConvention -> Basis -> Settlements -> Date -> [Date]
getSettlementDates conv b sts dt = map (rollDay conv) $ iterate nextDate dt
 where nextDate = T.addDays daysBetween
       daysBetween   = daysBetweenSettlements dt sts

daysBetweenSettlements :: Date -> Settlements -> Days
daysBetweenSettlements d sts 
  | T.isLeapYear y = floor $ 366.0 / fromIntegral sts
  | otherwise  = floor $ 365.0 / fromIntegral sts
  where (y,_,_) = WeekDate.toWeekDate d

rollDay :: RollConvention -> Date -> Date
rollDay conv date
  | 1 <= day && day <= 5 = date
  | otherwise = doRoll conv date
  where (_,_,day) = WeekDate.toWeekDate date

doRoll :: RollConvention -> Date -> Date
doRoll Following date = rollForward date
doRoll Preceding date = rollBackwards date
doRoll ModifiedFollowing date
  | mn' == mn = fwdDate
  | otherwise = rollBackwards date
  where (_, mn', _) = Cal.toGregorian fwdDate
        (_, mn , _)  = Cal.toGregorian date
        fwdDate = rollForward date

legalDay :: Date -> Bool -- TODO: Add argument so users may specify holidays 
legalDay date
  | 1 <= day && day <= 5 = True
  | otherwise = False
  where (_,_, day) = WeekDate.toWeekDate date

rollForward, rollBackwards :: Date -> Date
rollForward date
  | legalDay date = date
  | otherwise     = rollForward $ Cal.addDays 1 date
rollBackwards date
  | legalDay date = date
  | otherwise     = rollBackwards $ Cal.addDays (-1) date

--
-- Classes
-- 

class Instrument i where
  pv :: i -> InterestRate -> Date -> Cash
  expired  :: i -> Date -> Bool
  yrsToExpiry :: i -> Date -> Years
  cashflow :: i -> Payments

class Instrument b => Bond b where
  principal :: b -> Cash
  coupon    :: b -> Cash
  maturity  :: b -> Date
  ytm       :: b -> Date -> Payment
  duration  :: b -> Payment

class Instrument d => Derivative d where
  underlying :: (Instrument i) => d -> i

-- class (Instrument e) => Equity e where
-- class (Instrument o) => Option o where

{- Sample usage
    > date0 = (read "2012-01-31") :: Date
    > date1 = (read "2012-06-31") :: Date
    > bond0 = BaseBond date0 USD date1 10 0 ACT365F ModifiedFollowing 100
    > zero0 = zero bond0
    > zero0 = zero bond0
    > pv0   = pv zero0 0.12 date0
-}

date0 = (read "2012-01-31") :: Date
date1 = (read "2012-06-31") :: Date
a = (read "2013-12-07") :: Date
b = (read "2013-11-30") :: Date

--
-- Instances
--

discountPayment r p (Payment d (Cash _ c)) = c / ((1+r) ** yrs)
  where yrs = fromInteger (T.diffDays d p) / 365

instance Instrument Zero where
  pv z@(Zero (Payment d c)) r p = flip scale c $ recip ((1+r) ** yrsToExpiry z p)
  expired (Zero (Payment d c)) p = 0 >= T.diffDays p d
  yrsToExpiry (Zero (Payment d c)) p = fromInteger (T.diffDays d p) / 365
  cashflow (Zero payment) = [payment]

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

-- instance Bond Annuity where
-- instance Bond SimpleAnnuity where
-- instance Bond MortgageBackedBond where
