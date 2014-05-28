{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
-- |
-- Module:      Instruments.Instrument
-- Copyright:   (c) Johan Astborg, Andreas Bock
-- License:     BSD-3
-- Maintainer:  Andreas Bock <bock@andreasbock.dk>
-- Stability:   experimental
-- Portability: portable
--
-- Top-level class for financial instruments
module Instruments.Instrument where
import Utils.Currency

-- | Instrument is a base class for financial instruments
--class Instrument i where
--  type PricingEngine :: *
--  expired :: i -> IO Bool
--  pv      :: i -> PricingEngine -> IO Cash

-- | this should have the method that uses the instrument only
class Instrument i where
  expired :: i -> IO Bool
  
-- | this should have the method that uses the model only
class Model m where
  calculate :: Instrument i => i -> m -> Cash

-- | This should have method that uses both the instrument and model  
class (Instrument i, Model m) => PricingEngine i m where
  pv :: i -> m -> IO Cash
  
  
