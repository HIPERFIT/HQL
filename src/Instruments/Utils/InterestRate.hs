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

-- | A term structure is a yield curve constructed of
--- solely of zero rates.
newtype DiscreteTermStructure     = DiscreteTermStructure (M.Map Maturity Rate)
newtype LinearInterpolatedTermStructure = LinearInterpolatedTermStructure (M.Map Maturity Rate)
newtype AnalyticalTermStructure   = AnalyticalTermStructure (Offset -> Rate)

class TermStructure a where
  -- | Returns the yield for a maturity
  yieldAt :: a -> Maturity -> Maybe Rate

  -- | Returns the discount factor at an offset
  dfAt :: a -> Maturity -> Maybe Rate
  dfAt a m = fmap recip $ yieldAt a m
 
  -- | Returns the discount factor at an offset
  fwdRate :: a -> Maturity -> Maturity -> Maybe DiscountFactor
  fwdRate a m0 m1 = (/) <$> dfAt a m1 <*> dfAt a m0
  
  -- | Returns the discount factors given a list of offsets
  dfsAt :: a -> [Maturity] -> [Maybe Rate]
  dfsAt a ms = map (dfAt a) ms

instance TermStructure DiscreteTermStructure where
  yieldAt (DiscreteTermStructure ts) m = M.lookup m ts

instance TermStructure LinearInterpolatedTermStructure where
  yieldAt (LinearInterpolatedTermStructure ts) m = fndIdx m (M.assocs ts) >>= linear
    where fndIdx b (k:l:ns)
            | fst k < b && b < fst l = Just (k, l)
            | otherwise = fndIdx b (l:ns)
          fndIdx _ _ = Nothing
          linear ((x0, y0), (x1, y1)) = return $ y0 + (y1-y0)*((m-x0)/(x1-x0))
          
instance TermStructure AnalyticalTermStructure where
  yieldAt (AnalyticalTermStructure f) m = return $ f m
