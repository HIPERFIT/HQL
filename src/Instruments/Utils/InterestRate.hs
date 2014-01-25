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
-- data Frequency = Annual | Month | Semi | Quarter -- | Daily -- Problem!

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
  discountFactor _ = undefined
--   discountFactor (ExponentialRate r n) conv d0 d1 = (1/(1+(r*dcf)/n))**((diffTime d1 d0)*n) -- fromIntegral
--     where dcf = modifier n d0 d1
        -- n has to be the Frequence _data_ type, which combined with a dc convention , use dates instead of offset
        -- Report fodder!!!

instance InterestRate FlatRate where
  continuousRate (FlatRate r) = ContinuousRate $ exp r
  discountFactor (FlatRate r) = const $ recip r+1

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
