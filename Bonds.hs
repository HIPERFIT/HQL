{-# LANGUAGE GADTs, RankNTypes #-}
import qualified Data.Map as M
import qualified Currency as C
import TermStructure

--
-- Types
--

type Payment = C.Cash Double
type InterestRate = Double
type CashFlow = M.Map Date Payment
type Settlements = Integer

type DiscountFunction = forall a. (Instrument a) => a -> TermStructure -> Payment

--
-- Instruments
--

data CommonBond = CommonBond {
  cprincipal      :: Payment,
  ctermstructure  :: TermStructure
}

newtype Zero = Zero { zero :: CommonBond }

data Consol = Consol {
  cterm   :: TermStructure,
  ccoupon :: Double
}

data Annuity = Annuity {
    abase   :: CommonBond,
    acoupon :: Double
}

newtype SimpleAnnuity = SimpleAnnuity { annuity :: Annuity }

data MortgageBackedBond = MortgageBackedBond {
    mbobase :: CommonBond,
    serviceFee :: Payment
}

--
-- Classes
-- 

class Instrument i where
  pv :: i -> DiscountFunction -> Payment
  expired :: i -> Bool
  cashflow :: i -> CashFlow

class Instrument b => Bond b where
  principal :: b -> Payment
  coupon :: b -> Payment
  maturity :: b -> Date
  ytm :: b -> Payment
  duration :: b -> Payment
  discount :: b -> DiscountFunction

--
-- TODO: Instances
-- 

instance Instrument Zero where
  pv z = undefined
  cashflow z = undefined
  expired z = undefined

instance Bond Zero where
  principal z = undefined
  coupon z = undefined
  maturity z = undefined
  ytm z = undefined
  duration z = undefined
  discount z = undefined

-- instance Bond Consol where
-- instance Bond Annuity where
-- instance Bond SimpleAnnuity where
-- instance Bond MortgageBackedBond where

class Instrument d => Derivative d where
  underlying :: (Instrument i) => d -> i
-- class (Instrument e) => Equity e where
-- class (Instrument o) => Option o where
