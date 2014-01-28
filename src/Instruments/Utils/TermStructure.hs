-- Module:      Instruments.Utils.InterestRate
-- Copyright:   (c) 2013 HIPERFIT
-- License:     BSD-3
-- Maintainer:  Johan Astborg, Andreas Bock <bock@andreasbock.dk>
-- Portability: portable
--
-- Types and functions for working with interest rates.
module Instruments.Utils.TermStructure where

import Instruments.Utils.InterestRate
import Control.Applicative
import Data.Tuple
import Data.Maybe
import qualified Data.Map as M

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
  dfAt a m = return (1-) <*> yieldAt a m
 
  -- | Returns the discount factor at an offset
  fwdRate :: a -> Maturity -> Maturity -> Maybe DiscountFactor
  fwdRate a m0 m1 = (/) <$> dfAt a m1 <*> dfAt a m0
  
  -- | Returns the discount factors given a list of offsets
  dfsAt :: a -> [Maturity] -> [Maybe Rate]
  dfsAt a = map (dfAt a)

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
