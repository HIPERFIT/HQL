{-# LANGUAGE TypeFamilies, GADTs, RecordWildCards, RankNTypes, MultiParamTypeClasses #-}
-- Module:      Instruments.Swap.Swap
-- Copyright:   (c) Kasper Passov
-- Maintainer:  Kasper Passov <kasper.passov@gmail.com>
--
-- Types and functions for working with and evaluating swaps

module Instruments.Swaps.Swap where
import Instruments.Utils.InterestRate
import Instruments.Utils.FloatingRate
import Utils.Currency
import Utils.Calendar
import Instruments.Instrument

    
-- | the spread in bas points of the floating leg
type Spread = Double
type Nominal = Double
type Discount = Double 
type Seed = Double

    
-- VanillaSwap and legs


data Leg a = Fixed a Nominal [Date] | Floating FloatingRate Nominal [Date]

data VanillaSwap a = VanillaSwap (Leg a) (Leg a)

class (Instrument s) => Swap s where
    npv_fixed  :: Leg l => s -> l -> Cash
    npv_floating :: Leg l => s -> l -> Cash
    npv :: s -> Cash

instance InterestRate a => Swap (VanillaSwap a) where
    npv_fixed (Fixed fixedr nom schedule) = Cash 10 USD
    npv_floating (Floating floatingr nom schedule) = Cash 100 USD
    
    npv (VanillaSwap leg1@(Fixed _ _ _)    leg2@(Fixed _ _ _))    = npv_fixed leg1    - npv_fixed leg2
    npv (VanillaSwap leg1@(Floating _ _ _) leg2@(Fixed _ _ _))    = npv_floating leg1 - npv_fixed leg2
    npv (VanillaSwap leg1@(Fixed _ _ _)    leg2@(Floating _ _ _)) = npv_fixed leg1    - npv_floating leg2
    npv (VanillaSwap leg1@(Floating _ _ _) leg2@(Floating _ _ _)) = npv_floating leg1 - npv_floating leg2

instance Instrument (VanillaSwap a) where
    expired (VanillaSwap (Fixed _ _ _)(Floating _ _ _)) = undefined
    