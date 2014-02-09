-- Module:      Instruments.Utils.TermStructure
-- Copyright:   (c) Johan Astborg, Andreas Bock
-- License:     BSD-3
-- Maintainer:  Johan Astborg, Andreas Bock <bock@andreasbock.dk>
-- Portability: portable
--
-- Types and functions for term structures
module Instruments.Utils.TermStructure where

import Instruments.Utils.InterestRate
import Control.Applicative
import Data.Tuple
import Data.Maybe
import qualified Data.Map as M

-- | A term structure is a yield curve constructed of
--- solely of zero rates.
data TermStructure = DiscreteTermStructure (M.Map Maturity Rate)
                   | AnalyticalTermStructure (Offset -> Rate)
                   | LinearInterpolatedTermStructure (M.Map Maturity Rate)

-- | Returns the yield for a maturity
yieldAt :: TermStructure -> Maturity -> Maybe Rate
yieldAt (DiscreteTermStructure ts) m = M.lookup m ts
yieldAt (LinearInterpolatedTermStructure ts) m = fndIdx m (M.assocs ts) >>= linear
  where fndIdx b (k:l:ns)
          | fst k < b && b < fst l = Just (k, l)
          | otherwise = fndIdx b (l:ns)
        fndIdx _ _ = Nothing
        linear ((x0, y0), (x1, y1)) = return $ y0 + (y1-y0)*((m-x0)/(x1-x0))
yieldAt (AnalyticalTermStructure f) m = return $ f m

-- | Returns the discount factor at an offset
dfAt :: TermStructure -> Maturity -> Maybe Rate
dfAt a m = recip <$> (\y -> (1+y)**m) <$> yieldAt a m

-- | Returns the forward rate given two offsets
fwdRate :: TermStructure -> Maturity -> Maturity -> Maybe DiscountFactor
fwdRate t m0 m1 = (/) <$> dfAt t m1 <*> dfAt t m0

-- | Returns the discount factors given a list of offsets
dfsAt :: TermStructure -> [Maturity] -> [Maybe Rate]
dfsAt t = map (dfAt t)
