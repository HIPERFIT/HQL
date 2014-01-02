module Instruments.Utils.Discounting where
import Instruments.Utils.TermStructure
import Utils.Calendar
import Utils.Currency
import qualified Data.Map as M

-- Round a number f to n number of digits
rnd :: Integer -> Double -> Double
rnd n f = fromInteger (round $ f * (10 ^ nn)) / (10.0 ^^ nn)
	where nn = max 0 (n - floor(logBase 10 f) - 1)

-- |Discount Factors
df :: Double -> Rate -> Double -- DiscountFactor
df t r = exp(-r*t)

discountFactors :: Compounding -> Date -> TermStructure -> [Date] -> [Double]
discountFactors c now (Analytical f) ds =
  zipWith df offsets rates
  where offsets = map (getDayOffset now) ds
        rates = map (makeContinuous c . recip . f) offsets
discountFactors c now (Interpolated ts) ds =
  let
    offsets = map (getDayOffset now) ds -- TODO: Wrap in ErrorT monad
    rates = map (\d -> makeContinuous c $ ts M.! d) ds
  in
    zipWith df offsets rates

-- Tests
analyticalFun1 x = 5 + (1/2)*sqrt x

-- NTest 11
ts1  = Analytical analyticalFun1
now1 = read "2000-01-01" :: Date
mat1 = read "2004-07-05" :: Date
cmp1 = Exponential 2
df1  = discountFactors cmp1 now1 ts1 [mat1] -- TODO: Conversion is buggy,
                                            -- check `mc` in TermStructure

-- NTest 15
mat2 = read "2000-07-02" :: Date
ts2  = Interpolated $ M.fromList [(mat2, 0.083)]
cmp2 = Continuous
df2  = discountFactors cmp2 now1 ts2 [mat2]

-- tests = [df1 == 0.7643885607510086,
tests = [head df2 == 0.9593493353414723]

testAll = all (==True) tests
