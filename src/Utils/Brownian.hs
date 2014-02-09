{-# LANGUAGE GADTs, RankNTypes #-}
-- |
-- Module:      Utils.Brownian
-- Copyright:   (c) Johan Astborg, Andreas Bock
-- License:     BSD-3
-- Maintainer:  Andreas Bock <bock@andreasbock.dk>
-- Stability:   experimental
-- Portability: portable
--
-- Proof of concept of stochastic process simulation
module Utils.Brownian where

import Control.Monad.Random
import Control.Monad.State

type Simulation = State StdGen -- Lazy

-- | Generates a random number in the [0,1] range
randomUnit :: Simulation Double
randomUnit = state $ randomR (0,1)

-- | Converts a uniform distributin sample into a
-- normal gaussian distributed sample.
boxMuller :: Simulation Double
boxMuller = do
  u0 <- randomUnit
  return $ transform u0 u0
  where transform x y = sqrt(-2*log x )*cos(2*pi*y)

-- | A standard Brownian motion
sbm :: Int -> Simulation [Double]
sbm n = replicateM n boxMuller
