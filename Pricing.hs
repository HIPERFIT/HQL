module Pricing where
import Calendar
import Currency
import qualified Data.Time as T
import Bonds

--
-- Instances
--
discountPayment r p (Payment d (Cash _ c)) = c / ((1+r) ** yrs)
  where yrs = fromInteger (T.diffDays d p) / 365

instance Instrument Zero where
  pv z@(Zero (Payment d c)) r p = flip scale c $ recip ((1+r) ** yrsToExpiry z p)
  expired (Zero (Payment d c)) p = 0 >= T.diffDays p d
  cashflow (Zero payment) = [payment]
  yrsToExpiry (Zero (Payment d c)) p = fromInteger (T.diffDays d p) / 365 -- TODO: 365 is not always right, right?

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

instance Instrument Annuity where
  pv (Annuity ps@(Payment _ (Cash c _):_)) r p = Cash c $ sum $ map (discountPayment r p) ps
  expired b p = 0 >= (T.diffDays p $ maturity b)
  yrsToExpiry b p = fromInteger (T.diffDays (maturity b) p) / 365
  cashflow (Annuity ps) = ps

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

instance Bond Annuity where
  principal (Annuity (Payment d c:ps)) = undefined -- What do we do here..?
  coupon (Annuity (Payment _ c:_)) = c
  maturity _ = undefined -- what is the biggest UTCTime we have? :P
  ytm c = undefined
  duration b = undefined

-- instance Bond SimpleAnnuity where
-- instance Bond MortgageBackedBond where
