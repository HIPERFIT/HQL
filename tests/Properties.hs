import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..))
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Time.Clock (UTCTime(..))
import Data.Time (ZonedTime(..))

encodeInteger :: Integer -> Bool
encodeInteger i = (show i) == (show i)

main :: IO ()
main = defaultMain tests

-- Tests goes here
-- TODO: Update test cases...
tests :: [Test]
tests = [
	testGroup "Bonds" [
		testProperty "encodeInteger" encodeInteger
	],
	testGroup "Discounting" [
		testProperty "encodeInteger" encodeInteger
	],
	testGroup "TermStructure" [
		testProperty "encodeInteger" encodeInteger
	],
	testGroup "Calendar" [
		testProperty "encodeInteger" encodeInteger
	],
	testGroup "Currency" [
		testProperty "encodeInteger" encodeInteger
	]]
