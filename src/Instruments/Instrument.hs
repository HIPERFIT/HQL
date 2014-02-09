{-# LANGUAGE TypeFamilies #-}
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
class Instrument i where
  type PricingEngine :: *
  expired :: i -> IO Bool
  pv      :: i -> PricingEngine -> IO Cash
