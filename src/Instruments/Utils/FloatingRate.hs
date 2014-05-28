-- Module:      Instruments.Swap.Swap
-- Copyright:   (c) Kasper Passov
-- Maintainer:  Kasper Passov <kasper.passov@gmail.com>
--
-- Types and functions for working with and evaluating swaps
module Instruments.Utils.FloatingRate where
import Instruments.Utils.InterestRate
import System.Random(RandomGen)
import Utils.Calendar

class RateModel m where
    -- | returns the deterministic rate
    getRateDet  :: m -> Date -> Rate
    -- | returns the rate according to the model
    getRate :: RandomGen g => m -> g -> (Rate, g)
    isDet :: m -> Bool

data FloatingRate = LIBOR [(Maturity, Rate)]
                  -- | HullWhite
    
    
--instance RateModel FloatingRate where
  --  getRateDet 
