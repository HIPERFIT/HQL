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

-- | Instrument is a high-level class for financial instruments
class Instrument i where
  expired :: i -> IO Bool
