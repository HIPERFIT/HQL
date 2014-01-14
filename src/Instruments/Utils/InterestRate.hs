-- Module:      Instruments.Utils.InterestRate
-- Copyright:   (c) 2013 HIPERFIT
-- License:     BSD-3
-- Portability: portable
--
-- Types and functions for working with interest rates.
module Instruments.Utils.InterestRate where
import Control.Applicative
import qualified Data.Map as M

type Rate = Double
type Maturity = Double
type DiscountFactor = Double
type Offset = Double
type Frequency = Double

data Compounding = Continuous 
        		 | Exponential
		         | Linear deriving (Show)

newtype ContinuousRate = ContinuousRate Rate
newtype FlatRate       = FlatRate Rate
data ExponentialRate   = ExponentialRate Rate Frequency

class InterestRate a where
  -- | Returns the corresponding continuously compounded rate
  continuousRate :: a -> ContinuousRate

  -- | Returns the discount factor at an offset
  discountFactor :: a -> Offset -> DiscountFactor 

instance InterestRate ContinuousRate where
  continuousRate = id
  discountFactor (ContinuousRate r) offset = exp (-r*offset)

instance InterestRate ExponentialRate where
  continuousRate (ExponentialRate r n) = ContinuousRate $ (exp(r*n) - 1)/n
  discountFactor (ExponentialRate r n) offset = (1/(1+r*n))**(offset/n)

instance InterestRate FlatRate where
  continuousRate (FlatRate r) = ContinuousRate $ exp r
  discountFactor (FlatRate r) = const $ recip r+1

-- | A term structure is a yield curve constructed of
--- solely of zero rates.
newtype InterpolatedTermStructure = InterpolatedTermStructure (M.Map Maturity Rate)
newtype AnalyticalTermStructure = AnalyticalTermStructure (Offset -> Rate)

class TermStructure a where
  -- | Returns the yield for a maturity
  yieldAt :: a -> Maturity -> Maybe Rate

  -- | Returns the discount factor at an offset
  dfAt :: a -> Maturity -> Maybe Rate
  dfAt a m = fmap recip $ yieldAt a m
 
  -- | Returns the discount factor at an offset
  fwdRate :: a -> Maturity -> Maturity -> Maybe DiscountFactor
  fwdRate a m0 m1 = (/) <$> dfAt a m1 <*> dfAt a m0

instance TermStructure InterpolatedTermStructure where
  yieldAt (InterpolatedTermStructure ts) m = M.lookup m ts

instance TermStructure AnalyticalTermStructure where
  yieldAt (AnalyticalTermStructure f) m = return $ f m
