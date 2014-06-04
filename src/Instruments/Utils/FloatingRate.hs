{-# LANGUAGE GADTs #-}
-- Module:      Instruments.Swap.Swap
-- Copyright:   (c) Kasper Passov
-- Maintainer:  Kasper Passov <kasper.passov@gmail.com>
--
-- Types and functions for working with and evaluating swaps
module Instruments.Utils.FloatingRate where
import Instruments.Utils.InterestRate
import System.Random(RandomGen)
import Utils.Calendar
import Data.Maybe(fromJust)


data FloatingRate a where
    LIBOR :: {tabl :: Table a} -> FloatingRate a deriving(Show)
                  -- | HullWhite
    
type Table a = [(Maturity, a)]

--data MaturityFl = Overnight
--              | OneWeak
--              | OneMonth
--              | TwoMonths
--              | ThreeMonths
--              | SixMonths
--              | OneYear
--                deriving (Show, Eq)

class FloatingModel m where
    -- | returns the deterministic rate
    getRateTable  :: m -> Maturity -> Rate
    -- | returns the rate according to the model
    getRate :: RandomGen g => m -> g -> (Rate, g)
    isDet :: m -> Bool

instance InterestRate a => FloatingModel (FloatingRate a) where
    getRateTable (LIBOR (tabl)) matu = rate $ fromJust $ lookup matu tabl
    getRate (LIBOR _) = error "LIBOR is deterministic."
    isDet (LIBOR _) = True



