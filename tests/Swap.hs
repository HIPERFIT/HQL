import Instruments.Swaps.Swap
import Instruments.Instrument
import Instruments.Utils.InterestRate
import Instruments.Utils.TermStructure
import Control.Monad(liftM)
import Utils.Currency
import Utils.DayCount
import Utils.Calendar
import Data.Time
import Data.List

toda = getCurrentTime >>= \d -> return (utctDay d)
--toda = toGregorian $ utctDay getCurrentTime


p = Cash 100 DKK 
r = ExponentialRate 5.0 Annually
b = ACTACT
ds = [fromGregorian 2014 07 05, fromGregorian 2014 08 03, fromGregorian 2014 09 03, 
      fromGregorian 2014 10 03, fromGregorian 2014 11 03, fromGregorian 2014 12 03, 
      fromGregorian 2015 01 03, fromGregorian 2015 02 03, fromGregorian 2015 03 03, 
      fromGregorian 2015 04 03, fromGregorian 2015 05 03, fromGregorian 2015 06 03]
ds2 = [fromGregorian 2017 07 05,fromGregorian 2015 07 05,fromGregorian 2016 07 05, 
       fromGregorian 2014 07 05,fromGregorian 2018 07 05,fromGregorian 2019 07 05, 
       fromGregorian 2023 07 05,fromGregorian 2021 07 05,fromGregorian 2022 07 05, 
       fromGregorian 2020 07 05,fromGregorian 2024 07 05,fromGregorian 2025 07 05]
       
fixedLeg = FiLeg p ds2 r b

floatingLeg = FlLeg p ds2 b

termstruct = AnalyticalTermStructure (\x -> 1 + (1/2)*sqrt(x))

vanillaswapFloating = VanillaSwap floatingLeg fixedLeg

vanillaswapFixed = VanillaSwap fixedLeg floatingLeg

--libor = LIBOR [(fromGregorian 2000 01 01, SimpleRate 6.0)] 
--getRateDet libor (fromGregorian 2000 01 01) == 6.0