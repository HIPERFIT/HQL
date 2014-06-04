{-# LANGUAGE TypeFamilies, GADTs, RecordWildCards, RankNTypes, MultiParamTypeClasses #-}
-- Module:      Instruments.Swap.Swap
-- Copyright:   (c) Kasper Passov
-- Maintainer:  Kasper Passov <kasper.passov@gmail.com>
--
-- Types and functions for working with and evaluating swaps

module Instruments.Swaps.Swap where

import Control.Monad (liftM)
import Instruments.Utils.InterestRate
import Instruments.Utils.FloatingRate
import Utils.Currency as Cur
import Utils.Calendar
import Utils.DayCount
import Instruments.Instrument

    
-- | the spread in bas points of the floating leg
type Spread = Double
type Nominal = Double
type Discount = Double 
type Seed = Double
type InterestRateSwap = Double
    
data Leg a where 
    FiLeg :: {prin :: Cash,
              inra :: a,
              accr :: Basis, -- accrual factor
              dats :: [Date] } -> Leg a
    FlLeg :: {prin :: Cash,
              inra :: a, -- FloatingModel
              accr :: Basis,
              matu :: Maturity,
              flra :: FloatingRate a, -- How do i make this return a?
              dats :: [Date] } -> Leg a
    
data VanillaSwap a where
     VanillaSwap :: {l1 :: Leg a, 
                     l2 :: Leg a} -> VanillaSwap a

class Instrument s => Swap s where

instance InterestRate a => Swap (VanillaSwap a) where
    --npv leg1 leg2 = npv_leg leg1 - npv_leg leg2

instance InterestRate a => Instrument (VanillaSwap a) where
    type PricingEngine = InterestRateSwap 
    expired _ = undefined
    pv VanillaSwap{..} = undefined --npv_leg l1 - npv_leg l2
    

pv_leg :: InterestRate a => Leg a -> Cash
pv_leg FlLeg{..} = Cur.sum . map (\(dt0, dt1) -> 
                let accrualfact = modifier accr dt0 dt1 in
                let fora dt0 dt1 = (1/accrualfact) * ((discountFactor inra $ modifier accr dt0 dt0) / 
                                                      (discountFactor inra accrualfact) - 1) in
                   Cur.scale ((discountFactor inra $ modifier accr dt0 dt1) * (fora dt0 dt1)) $ prin) $ getPairs dats
                        
pv_leg FiLeg{..} = Cur.sum . map (\(dt0, dt1) -> 
                   let accrualfact = modifier accr dt0 dt1 in
                   Cur.scale accrualfact $ Cur.scale (discountFactor inra accrualfact) interest)          $ getPairs dats
                   where interest = Cur.scale (rate inra / 100) prin
                    
getPairs :: [a] -> [(a, a)]
getPairs [] = []
getPairs (_:[]) = []
getPairs list = (head list, head $ tail list) : getPairs(tail list)

instance Show (Leg a) where
    show FiLeg{..} = "Rate = 10"
    show FlLeg{..} = "Principal = 10000000" 
