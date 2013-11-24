{-# LANGUAGE GADTs, RankNTypes #-}
module Bonds where
import qualified Data.Map as M
import qualified Data.Time as T
import Currency
import TermStructure
import Control.Monad
import Prelude hiding (exp)

--
-- Types
--

type InterestRate = Double
type Years = Double -- Can be fractional
type Payment = Cash Double
type Payments = M.Map Date Payment
type Settlements = Int

type DiscountFunction = forall a. (Instrument a) => a -> TermStructure -> Payment

--
-- Instruments
--

data Vanilla = Vanilla {
  faceValue    :: Payment,
  bondMaturity :: Date,
  interest     :: InterestRate
}

newtype Zero = Zero { zero :: Vanilla }

data Bullet = Bullet { bullet :: Vanilla }

data Annuity = Annuity {
  abase     :: Vanilla,
  acashflow :: Payment -- repayment + coupon
}

data Serial = Serial {
  sbase     :: Vanilla,
  repayment :: Payment -- fixed for serial bonds
}

data Consol = Consol {
  cinterest   :: InterestRate,
  settlements :: Payment 
}

newtype SimpleAnnuity = SimpleAnnuity { annuity :: Annuity }

data MortgageBackedBond = MortgageBackedBond {
    mbobase    :: Vanilla,
    serviceFee :: Payment
}

--
-- Classes
-- 

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

{-
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
