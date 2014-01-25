-- Module:      Instruments.Utils.InterestRate
-- Copyright:   (c) 2013 HIPERFIT
-- License:     BSD-3
-- Maintainer:  Johan Astborg, Andreas Bock <bock@andreasbock.dk>
-- Portability: portable
--
-- Types and functions for working with interest rates.
module Instruments.Utils.InterestRate where
import Control.Applicative
import Data.Tuple
import Data.Maybe
import qualified Data.Map as M

type Rate = Double
type Maturity = Double
type DiscountFactor = Double
type Offset = Double

data Compounding = Continuous 
        	     | Exponential
		         | Linear 
                 deriving (Show)

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
newtype FlatRate       = FlatRate Rate deriving (Show)
data ExponentialRate   = ExponentialRate Rate Frequency deriving (Show)

class InterestRate a where
  -- | Returns the corresponding continuously compounded rate
  continuousRate :: a -> ContinuousRate

  -- | Returns the discount factor at an offset
  discountFactor :: a -> Offset -> DiscountFactor 

  -- | Annuallize the rate
  --annuallize :: a -> a TODO

  -- | Get the intrinsic rate
  rate :: a -> Rate


instance InterestRate ContinuousRate where
  continuousRate = id
  discountFactor (ContinuousRate r) offset = exp (-r*offset)
  rate (ContinuousRate r) = r

instance InterestRate ExponentialRate where	 
  continuousRate (ExponentialRate r n) = ContinuousRate $ (exp(r*nn) - 1)/nn 
		where nn = convertFreq n
  discountFactor (ExponentialRate r n) offset = (1/(1+r*nn))**(offset/nn) 
		where nn = convertFreq n
  rate (ExponentialRate r _) = r
	
instance InterestRate FlatRate where
  continuousRate (FlatRate r) = ContinuousRate $ exp r
  discountFactor (FlatRate r) = const $ recip r+1
  rate (FlatRate r) = r
