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
import Instruments.Utils.InterestRate

--import Instruments.Utils.TermStructure
--import Instruments.Utils.Discounting

main :: IO ()
main = defaultMain tests

-- Code to test for almost equal using Data.AEq in IEEE-754
(@=~?) :: (Show a, AEq a) => a -> a -> Assertion
(@=~?)  actual expected = expected ~== actual @? assertionMsg
    where
      assertionMsg = "Expected : " ++ show expected ++
                     "\nActual   : " ++ show actual

----------- Interest rates (values from Table 3 in documentation -- In percentage)
interestRate1 = (rate $ continuousRate $ ExponentialRate 1.0000 Annually) @=~? 1.005016708416795
interestRate2 = (rate $ continuousRate $ ExponentialRate 1.0025 SemiAnnually) @=~? 1.005016718885754
interestRate3 = (rate $ continuousRate $ ExponentialRate 1.0038 Quarterly) @=~? 1.0050605722981665
interestRate4 = (rate $ continuousRate $ ExponentialRate 1.0046 Monthly) @=~? 1.0050206261866812

--interestRate1 = continuousRate (SimpleSpotRate 1.0025 (1/2)) @=~? 1.005016718885754
--interestRate2 = continuousRate (SimpleSpotRate 5.0625 (1/2)) @=~? 5.127116313804603
--interestRate3 = continuousRate (CompoundedSpotRate 15.0000 1) @=~? 16.183424272828304
--interestRate4 = continuousRate (CompoundedSpotRate 21.5506 (1/4)) @=~? 22.14170314225603
--interestRate5 = continuousRate (ContinuousSpotRate 22.1403 0) @=~? 22.1403

----------- Discount Factor

--discountFactor1 = df (discountFactor (ContinuousSpotRate 8.3 0.5) 0.5) @=~? 0.9593493353414723
--discountFactor2 = df (discountFactor (CompoundedSpotRate 8.5 0.5) 0.5) @=~? 0.9592326139088729
--discountFactor3 = df (discountFactor (CompoundedSpotRate 8.5 (1/12)) 3) @=~? 0.7756133702070988
--discountFactor4 = df (discountFactor (SimpleSpotRate 5.6124 (1/2)) 1.5) @=~? 0.9203268231254242

{-
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
		testCase "interestRate3" interestRate3,
		testCase "interestRate3" interestRate4
		--testCase "interestRate3" interestRate5
		-- TODO: Add test cases
	],
	testGroup "Discounting" [
		--testCase  "discountFactor1" discountFactor1,
		--testCase  "discountFactor2" discountFactor2,
		--testCase  "discountFactor3" discountFactor3,
		--testCase  "discountFactor4" discountFactor4
		--testCase  "discountFactor5" discountFactor5
	],
	testGroup "TermStructure" [
	],
	testGroup "Calendar" [
		{-
		Add Test Cases for:
		  - Day count convetions
		  - Date diff using conventions
		-}
		
	],
	testGroup "Currency" [
	]]
