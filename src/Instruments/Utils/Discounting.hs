-- module Discounting where
module Discounting where
import TermStructure
import Prelude

-- Round a number f to n number of digits
rnd n f = fromInteger (round $ f * (10 ^ nn)) / (10.0 ^^ nn)
	where nn = max 0 (n - floor(logBase 10 f) - 1)

-- |Discount Factors
df t maturityT r = exp(-(r/100.0)*(maturityT - t))
discountFactor r maturityT t = df t maturityT r

-- |Lazy lists Discount Factors from rate, rates of term structure.
-- will repeat the input sequence to infinity
discountFactors (InterestRate Continuous r) offsetT =
  zipWith (df offsetT) [offsetT+1..] (repeat r)
discountFactors (InterestRates Continuous rs) offsetT =
  zipWith (df offsetT) [offsetT+1..] (cycle rs)
discountFactors (TermStructureRates Continuous (Analytical ts)) offsetT =
  map (\t ->discountFactor (ts t) t offsetT) [offsetT+1..]

discountFactors (InterestRate (Periodic p) r) offsetT =
  discountFactors (InterestRate Continuous (mc p r)) offsetT
discountFactors (InterestRates (Periodic p) rs) offsetT =
  discountFactors (InterestRates Continuous (map (mc p) rs)) offsetT
discountFactors (TermStructureRates (Periodic p) (Analytical ts)) offsetT =
  map (\t ->discountFactor (mc p (ts t)) t offsetT) [offsetT+(1/pp)..]
  where pp = fromIntegral p

-- EXPERIMENTAL
analyticalFun1 x = 5 + (1/2)*sqrt x

-- Finite list of cash flows, merging coupons and nominal at maturity
cfs n c f = [c | i <- [0..n-2]] ++ [f+c]

-- PV takes two lists cfs and dfs
pv cfs dfs = sum $ zipWith (*) cfs dfs
fv cfs dfs = sum $ zipWith (/) cfs dfs

-- Test cases for discounting
input = map (rnd 6) [
		
	-- Example - Fixed Coupon Bond (see documentation)
	-- Maturity: 5 years
	-- Coupon: 100
	-- Face: 1000
	-- Interest rates: 3.00%, 3.25%, 3.50%, 3.55%, 3.30%
	-- PV: time t=0
	pv (cfs 5 100 1000) (take 5 $ discountFactors (InterestRates (Periodic 1) [3.0,3.25,3.5,3.55,3.3]) 0)
	--pv (cfs 5 100 1000) (take 5 $ dfs2 [3.0,3.25,3.5,3.55,3.3] 0) -- 1303.2324619858493
	,
	
	-- Example - Annuity (see documentation)
	-- Maturity: 5 years
	-- Coupon: 1000
	-- Face: 0
	-- Interest rate: 5.00%
	--pv (cfs 5 1000 0) (take 5 $ dfs 5 0) -- 4329.476670630818
	pv (cfs 5 1000 0) (take 5 $ discountFactors (InterestRate (Periodic 1) 5) 0)
	,
	
	-- USING TERMSTRUCTURE
	
	-- Example - Discount factor (DE test 7)
	-- Maturity: 1/2 years
	-- Interest rate: term structure
	-- Compounding: 2 / year
	discountFactors (TermStructureRates (Periodic 2) (termStructure analyticalFun1)) 0 !! 1
	--df (mc3 (termStructure 1.5) 2) 1.5 -- 0.9203271932613013
	--(take 6 $ discountFactors (TermStructureRates (Periodic 2) termStructure) 0)
	] 

-- Expected outout
expected = 
	[1303.23,4329.48,0.920327]
	
test = input == expected 
