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

-- | I made a leg class so i can call npv_leg regardless of legtype
class Leg l where
    npv_leg :: l -> Cash

--data VanillaLeg a = FixedLeg a | FloatingLeg    

data FixedLeg a = Fixed a Nominal [Date]    

-- | i need interestrate    
instance InterestRate a => Leg (FixedLeg a) where
    npv_leg (Fixed rate nom dates) = undefined

-- | this one gets an a so it looks like fixedleg    
data FloatingLeg a = Floating FloatingRate Nominal [Date]

instance Leg FloatingLeg where
    npv_leg (Floating rate nom dates) = undefined

--data VanillaLeg a = FixedLeg a | FloatingLeg    

-- we still want 2 legs    
data VanillaSwap a = VanillaSwap (Leg a) (Leg a)


class Instrument s => Swap s where
    npv :: s -> Cash

instance InterestRate a => Swap (VanillaSwap a) where
    npv _ = undefined
    --npv leg1 leg2 = npv_leg leg1 - npv_leg leg2

instance Instrument (VanillaSwap a) where
    expired _ = undefined
    