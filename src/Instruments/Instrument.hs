{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:      Instruments.Instrument
-- Copyright:   (c) 2013 HIPERFIT
-- License:     BSD-3
-- Maintainer:  Andreas Bock <bock@andreasbock.dk>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working with interest rates
module Instruments.Instrument where
import Utils.Currency

-- | Instrument is a high-level class for financial instruments
-- class Instrument i where
--   expired :: i -> IO Bool

-- | Instrument is a high-level class for financial instruments
class Instrument i where
  type PricingEngine :: *
  expired :: i -> IO Bool
  pv      :: i -> PricingEngine -> IO Cash
