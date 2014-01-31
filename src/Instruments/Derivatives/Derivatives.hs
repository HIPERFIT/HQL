-- |
-- Module:      Instruments.Derivatives.Derivatives
-- Copyright:   (c) 2013 HIPERFIT
-- License:     BSD-3
-- Maintainer:  Andreas Bock <bock@andreasbock.dk>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working with interest rates
module Instruments.Derivatives.Derivatives where

import Instruments.Instrument

-- The following class is purely proof of concept
class Instrument d => Derivative d where
  data Underlying :: *
  underlying :: d -> Underlying
