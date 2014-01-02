import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..))
--import Test.QuickCheck
--import Test.QuickCheck.All
import qualified Data.Vector as V
--import qualified Data.Attoparsec.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L
--import qualified Data.Text as T
--import qualified Data.HashMap.Strict as H
import Data.Time.Clock (UTCTime(..))
import Data.Time (ZonedTime(..))
--import Instances ()
--import Types
--import Encoders
--
encodeInteger :: Integer -> Bool
encodeInteger i = (show i) == (show i)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
	testGroup "encode" [
		testProperty "encodeInteger" encodeInteger
	]]
