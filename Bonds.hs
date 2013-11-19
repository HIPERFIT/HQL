{-# LANGUAGE GADTs, RankNTypes #-}
import qualified Data.Map as M
import qualified Data.Time as T
import qualified Currency as C
import TermStructure
import Control.Monad

--
-- Types
--

type Payment = C.Cash Double
-- type Payment = C.Cash 
type InterestRate = Double
type CashFlow = M.Map Date Payment
type Settlements = Int

type DiscountFunction = forall a. (Instrument a) => a -> TermStructure -> Payment

--
-- Instruments
--

data CommonBond = CommonBond {
  cprincipal      :: Payment,
  ctermstructure  :: TermStructure
}

data Zero = Zero { 
  faceValue :: Payment,
  zmaturity :: Date,
  rate      :: Double,
  zsettle   :: Settlements
}

newtype Bullet = Bullet { bullet :: CommonBond }

newtype Serial = Serial { serial :: CommonBond }

data Consol = Consol {
  crate    :: Double,
  ccoupon  :: Payment
--   cterm   :: TermStructure,
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
  pv :: Date -> i -> Payment
  expired :: Date -> i -> Bool
  cashflow :: i -> CashFlow

class Instrument b => Bond b where
  principal :: b -> Payment
  coupon :: b -> Payment
  maturity :: b -> Date
  ytm :: b -> Payment
  duration :: b -> Payment
  discount :: b -> DiscountFunction -- Does this function make sense? 

class Instrument d => Derivative d where
  underlying :: (Instrument i) => d -> i

-- class (Instrument e) => Equity e where
-- class (Instrument o) => Option o where

--
-- Instances
-- 

instance Instrument Zero where
  pv p (Zero f m r s)
    | T.diffDays m p > 365 = let
                               yrs = T.diffDays m p `mod` 365
                               exponent = toInteger s * yrs
                               s'       = fromIntegral s
                             in
                               f C./. (1+r/s')^exponent -- Day-count conventions!
    | otherwise = undefined -- get daily rate and recompound
  expired p (Zero f m z s) = 0 >= T.diffDays m p
  cashflow (Zero f m _ _) = M.insert m f M.empty -- only receive `f` on maturity

instance Instrument Consol where
  pv p (Consol r c) = c C./. r
  expired _ _ = False
  cashflow (Consol ts c) = undefined

instance Bond Zero where
  principal (Zero f _ _ _) = f
  coupon = const 0
  maturity (Zero _ m _ _) = m
  ytm z = undefined
  duration z = undefined
  discount (Zero f m r s) ts = undefined

instance Bond Consol where
  principal (Consol ts coupon) = undefined
  coupon (Consol _ c) = c
  maturity _ =  undefined -- what is the biggest UTCTime we have? :P
  ytm c = undefined
  duration b = undefined
  discount b = undefined

-- instance Bond Annuity where
-- instance Bond SimpleAnnuity where
-- instance Bond MortgageBackedBond where
