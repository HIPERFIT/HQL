import Instruments.Swaps.Swap
import Instruments.Utils.InterestRate
import Instruments.Utils.FloatingRate
import Utils.Currency
import Utils.DayCount
import Data.Time.Calendar


p = Cash 20 DKK 
r = ExponentialRate 6.0 Annually
b = ACTACT
ds = [fromGregorian 2010 03 03, fromGregorian 2010 04 03, fromGregorian 2010 05 03, 
      fromGregorian 2010 06 03, fromGregorian 2010 07 03, fromGregorian 2010 08 03, 
      fromGregorian 2010 09 03, fromGregorian 2010 10 03, fromGregorian 2010 11 03, 
      fromGregorian 2010 12 03, fromGregorian 2011 01 03, fromGregorian 2011 02 03, 
      fromGregorian 2011 03 03]
legtest = FiLeg p r b ds

ds2 = [fromGregorian 2010 03 03, fromGregorian 2011 03 03]

legtest2 = FiLeg p r b ds2

--pv legtest legtest2 --

--libor = LIBOR [(fromGregorian 2000 01 01, SimpleRate 6.0)] 
--getRateDet libor (fromGregorian 2000 01 01) == 6.0