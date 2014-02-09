-- Module:      Instruments.Utils.InterestRate
-- Copyright:   (c) Johan Astborg, Andreas Bock
-- License:     BSD-3
-- Maintainer:  Johan Astborg <joastbg@gmail.com>
-- Portability: portable
--
-- Types and functions for working with interest rates,
-- compounding and discounting.

module Instruments.Utils.InterestRate where
import Control.Applicative
import Data.Tuple
import Data.Maybe
import qualified Data.Map as M

type Rate = Double
type Maturity = Double
type DiscountFactor = Double
type CompoundFactor = Double
type Offset = Double

-- | Represents the type of compounding
--
-- For discrete exponential compounding, use Exponential.
-- For discrete simple compounding use Simple.
-- Continuous compounding is the default used in HQL internally.
data Compounding = Continuous
        	     | Exponential
		         | Linear
                 deriving (Show)

-- | Represents the compounding frequency
--
-- An interest rate can have different compounding frequencies.
-- For other frequencies than specified, use Other.
data Frequency = Annually
                 | SemiAnnually
                 | Quarterly
                 | Monthly
                 | Daily
                 | Other Int
		         deriving (Show, Eq)

convertFreq :: Frequency -> Double
convertFreq Annually     = 1
convertFreq SemiAnnually = 2
convertFreq Quarterly    = 4
convertFreq Monthly      = 12
convertFreq Daily        = 365
convertFreq (Other d)    = fromIntegral d

newtype ContinuousRate = ContinuousRate Rate deriving (Show)
newtype SimpleRate     = SimpleRate Rate deriving (Show)
data ExponentialRate   = ExponentialRate Rate Frequency deriving (Show)

class InterestRate a where
  -- | Returns the corresponding continuously compounded rate
  continuousRate :: a -> ContinuousRate

  -- | Returns the discount factor at an offset
  discountFactor :: a -> Offset -> DiscountFactor

  -- | Returns the compound factor at an offset
  compoundFactor :: a -> Offset -> CompoundFactor

  -- | Get the intrinsic rate
  rate :: a -> Rate

instance InterestRate ContinuousRate where
  continuousRate = id
  discountFactor (ContinuousRate r) offset = exp (-rr*offset) where rr = r / 100.0
  compoundFactor rate offset = 1 / discountFactor rate offset
  rate (ContinuousRate r) = r

instance InterestRate ExponentialRate where
  continuousRate (ExponentialRate r n) = ContinuousRate $ (exp(r/(100*nn)) - 1)*nn*100
		where nn = convertFreq n;
  discountFactor (ExponentialRate r n) offset = 1/((1+r/(100.0*nn))**(offset/nn))
		where nn = convertFreq n;
  compoundFactor rate offset = 1 / discountFactor rate offset
  rate (ExponentialRate r _) = r

instance InterestRate SimpleRate where
  continuousRate (SimpleRate r) = ContinuousRate $ (exp (r/100.0) - 1) * 100.0
  discountFactor (SimpleRate r) = const $ 1/(1+r/100.0)
  compoundFactor rate offset = 1 / discountFactor rate offset
  rate (SimpleRate r) = r
