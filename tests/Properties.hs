import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

import Test.QuickCheck (Arbitrary(..))
import Test.HUnit.Base hiding (Test)

import Data.AEq

import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as L

import Data.Time.Clock (UTCTime(..))
import Data.Time (ZonedTime(..))

import qualified Instruments.Utils.InterestRate as IR
import Instruments.Utils.TermStructure
import Instruments.Utils.Discounting

main :: IO ()
main = defaultMain tests

encodeInteger :: Integer -> Bool
encodeInteger i = (show i) == (show i)

--termStructure x = 5 + (1/2)*sqrt(x)

-- TESTS (1-17 non-calendar time)

--instance AEq Double where
--    x =~ y = abs ( x - y ) < (1.0e-8 :: Double)

-- Code to test for almost equal using Data.AEq in ieee754
(@=~?) :: (Show a, AEq a) => a -> a -> Assertion
(@=~?)  actual expected = expected ~== actual @? assertionMsg
    where
      assertionMsg = "Expected : " ++ show expected ++
                     "\nActual   : " ++ show actual

cfs n c f = [c | i <- [0..n-2]] ++ [f+c]
pv cfs dfs = sum $ zipWith (*) cfs dfs

----------- Interest rates

interestRate1 = IR.rate (IR.toContinuous (IR.InterestRate 1.0 IR.Compounded IR.Annually)) @=~? 1.005016708416795
interestRate2 = IR.rate (IR.toContinuous (IR.InterestRate 5.0625 IR.Compounded IR.SemiAnnually)) @=~? 5.127116313804603
interestRate3 = IR.rate (IR.toContinuous (IR.InterestRate 10.4713 IR.Compounded IR.Monthly)) @=~? 10.517119897313698

--------------------------


-- TEST 5
--discountFactor5 = discountFactor (InterestRate (Periodic Annually) (9+28/100 :: Double)) 1.00 0.0 == 0.9150805270863837
analyticalFun1 x = 5 + (1/2)*sqrt x

discountFactor1 = (discountFactors' (InterestRate (Exponential 1) (9+28/100 :: Double)) 0.0 0.0 !! 0) @=~? 0.915080527086383 

discountFactor2 = (discountFactors' (InterestRate (Exponential 12) 8.5) 0.0 0.0 !! 2) @=~? 0.77561337020709

discountFactor3 = (discountFactor' 8.3 0.5 0.0 0.0) @=~? 0.95934933534147

discountFactor4 = (discountFactors' (InterestRate Continuous 8.3) 0.0 0.5 !! 0) @=~? 0.95934933534147

-- Doesn't work because of interest rate conversions... Need more test cases for interests
--discountFactor5 = (discountFactors' (InterestRate (Exponential 2) 8.5) 0.5 0.0 !! 0) @=~? 0.95923261390887

--discountFactor2 = ((InterestRate (Exponential 2) (termStructure 1.5)) 1.5 0.0) == 0.9203271932613013
--discountFactor3 = ((discountFactors' (InterestRate Continuous 8.3) 0.5 0 == 0.9593493353414723 

--discountFactor6 = 

--discountFactor5 = (pv (cfs 5 100 1000) (take 5 $ discountFactors' (InterestRates (Exponential 1) [3.0,3.25,3.5,3.55,3.3]) 0)) @=~? 1303.232461985
--discountFactor5 = discountFactors' (InterestRate (Exponential 2)) (Analytical analyticalFun1) 0 !! 1 == 0.9203271932613013

--mat2 = read "2000-07-02" :: Date
--ts2  = Interpolated $ M.fromList [(mat2, 0.083)]
--cmp2 = Continuous
--df2  = discountFactors cmp2 now1 ts2 [mat2]

--discountFactor6 = (discountFactors Continuous (read "2000-01-01" :: Date) (Interpolated $ M.fromList [(mat2, 0.083)]) [read "2000-07-02" :: Date]) == 0.9593493353414723

-- Bonds
--bondTest1 = (dirty zero ts1 Continuous present) @?= (Cash 70.56481405950817 SEK)

{-
discountFactor (InterestRate (Exponential 1) (9+28/100 :: Double)) 1.00 0.0 == 0.915080527086383
discountFactor (InterestRate (Exponential 2) (termStructure 1.5)) 1.5 0.0 == 0.9203271932613013
discountFactor (InterestRate (Exponential 12) 8.5) 3.0 0.0 == 0.7756133702070988
discountFactor (InterestRate (Exponential 12) 8.5) 3.0 0.0 == 0.7756133702070988
discountFactor (InterestRate Continuous 8.3) 0.5 0 == 0.9593493353414723
discountFactor (InterestRate (Exponential 2) r) 4.50 0.0 == 0.7643885607510086
discountFactor (InterestRate (Exponential 2) (termStructure 4.5)) 4.5 0.0 == 0.7643885607510086
discountFactor (InterestRate (Exponential 2) 8.5) 0.5 0.0 == 0.9592326139088729
discountFactor (InterestRate (Exponential 2) 8.5) 0.5 0.0 == 0.9592326139088729
discountFactor (InterestRate Continuous 8.3) 0.5 0.0 == 0.9593493353414723
discountFactor (InterestRate Continuous 8.3) 0.5 0.0 == 0.9593493353414723
discountFactor (InterestRate Continuous (termStructure (90/360 :: Double))) (90/360 :: Double) 0.0 == 0.9869607572146836

-- FORWARD RATE
n = 2;
theta1 = 0.25;
theta2 = 1.5;
interest1 = 6.65;
interest2 = 7.77;
Out[8]= 7.99473

## HQL
> (((discountFactor (InterestRate (Periodic 2) 6.65) 0.25 0.0)/(discountFactor (InterestRate (Periodic 2) 7.77) 1.5))**(1/(2*1.25))-1)*2*100 == 7.994727369824384
-}

-- Tests goes here
-- TODO: Update test cases...
tests :: [Test]
tests = [
	testGroup "Bonds" [
		--testCase "bondTest1" bondTest1
	],
	testGroup "InterestRate" [
		testCase "interestRate1" interestRate1,
		testCase "interestRate2" interestRate2,
		testCase "interestRate3" interestRate3
		-- TODO: Add test cases
	],
	testGroup "Discounting" [
		testCase  "discountFactor1" discountFactor1,	
		testCase  "discountFactor2" discountFactor2,
		testCase  "discountFactor3" discountFactor3,
		testCase  "discountFactor4" discountFactor4
		--testCase  "discountFactor5" discountFactor5
	],
	testGroup "TermStructure" [
		testProperty "encodeInteger" encodeInteger
	],
	testGroup "Calendar" [
		testProperty "encodeInteger" encodeInteger
		{-
		Add Test Cases for:
		  - Day count convetions
		  - Date diff using conventions
		-}
		
	],
	testGroup "Currency" [
		testProperty "encodeInteger" encodeInteger
	]]
