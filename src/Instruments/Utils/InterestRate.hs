--{-# LANGUAGE DataKinds #-}

-- |
-- Module:      Instruments.Utils.InterestRate
-- Copyright:   (c) 2013 HIPERFIT
-- License:     BSD-3
-- Maintainer:  Johan Astborg <joastbg@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working with interest rates.

module Instruments.Utils.InterestRate where
--import Utils.DayCount
import qualified Data.Map as M

type Number = Double
type Offset = Double
type Rate = Double
type ContinuousRate = Double
type AnnualizedRate = Double
type Time = Double
type Term = Double
type Tenor = Double
type Maturity = Double


--------------------- MOVE this...

data DiscountFactor = DiscountFactor Number deriving (Show)

class DiscountFactorInstrument a where
    df :: a -> Rate

instance DiscountFactorInstrument DiscountFactor where
    df (DiscountFactor r) = r

---------------------


-- | Represents the compounding frequency
-- 
-- An interest rate can have different compounding frequencies.
-- For other frequencies than specified, use OtherFrequency.
data Frequency = Once -- For Zero Bond
               | Annually
               | SemiAnnually
               | Quarterly
               | Monthly
	           | NoFrequency 
               | OtherFrequency Int deriving (Show)

-- | Represents the type of compounding
--
-- For discrete compounding, use Compounded
-- For simple compounding, eg together with Once, use Simple
-- Continuous compounding is the default used in HQL internally.
data Compounding 
         = Continuous 
		 | Compounded 
		 | Simple 
		 | SimpleThenCompounded deriving (Show)

----------- InterestRateInstrument

data InterestRate 
          = SimpleSpotRate Rate Term
	      | ContinuousSpotRate Rate Term
          | CompoundedSpotRate Rate Term
          | SimpleForwardRate Rate Term Tenor
	      | ContinuousForwardRate Rate Term Tenor
          | CompoundedForwardRate Rate Term Tenor
	      deriving (Show)

class InterestRateInstrument a where
    continuousRate :: a -> ContinuousRate
    annualizedRate :: a -> AnnualizedRate
    -- | Returns the discount factor at an offset
    discountFactor :: a -> Offset -> DiscountFactor 
    -- | A lazy list of discount factors starting at an offset
    -- using the spacing implied by the conventions
    --discountFactors :: a -> Offset -> [DiscountFactor]

instance InterestRateInstrument InterestRate where

    -- | InterestRateInstrument
    continuousRate (SimpleSpotRate r termN) = (1/termN)*(exp(r/(100*(1/termN))) - 1)*100
    continuousRate (ContinuousSpotRate r _) = r
    continuousRate (CompoundedSpotRate r termN) = (1/termN)*(exp(r/(100*(1/termN))) - 1)*100
    
-- | ForwardRateInstruments
    continuousRate (SimpleForwardRate r _ _) = r
    continuousRate (ContinuousForwardRate r _ _) = r
    continuousRate (CompoundedForwardRate r _ _) = r

    -- | InterestRateInstrument
    discountFactor (SimpleSpotRate r termN) offsetT = DiscountFactor (1/(1+r/(100*(1/termN)))**(offsetT*(1/termN))) 
    discountFactor (ContinuousSpotRate r termN) offsetT = DiscountFactor (exp(-r/100*offsetT))
    discountFactor (CompoundedSpotRate r termN) offsetT = DiscountFactor (1/(1+r/(100*(1/termN)))**(offsetT*(1/termN))) 
    
-- | ForwardRateInstruments    
    -- ...

    -- | InterestRateInstrument
    annualizedRate (SimpleSpotRate r _) = r
    annualizedRate (ContinuousSpotRate r _) = r
    annualizedRate (CompoundedSpotRate r _) = r
   
    -- | ForwardRateInstruments    
    -- ...

-- | A term structure is a generalized yield curve 
-- which is a construct of yields at different maturities

type InterestRateCurve = M.Map Maturity InterestRate-- Assumes zero rates
type InterpolatedTermStructure = Double -> Double
type AnalyticalTermStructure = Double -> Double

data TermStructure = Interpolated InterpolatedTermStructure 
                   | Analytical AnalyticalTermStructure 

class TermStructureFunction a where

    -- | Returns the yield for a maturity
    yieldAt :: a -> Maturity -> InterestRate

    -- | Returns the discount factor at an offset
    --toDiscountFactor :: a -> Offset -> DiscountFactor

    -- | A lazy list of discount factors starting at an offset
    -- using the spacing implied by the conventions
    --toDdiscountFactors :: a -> Offset -> [DiscountFactor]
 
instance TermStructureFunction TermStructure where
    yieldAt (Interpolated zcts) maturityM = SimpleSpotRate 0.0 0.0 
    yieldAt (Analytical zcts) maturityM = SimpleSpotRate 0.0 0.0

    --toDiscountFactor (Interpolated zcts) offsetT = discountFactor offsetT (yieldAt zcts)
    --toDiscountFactor (Analytical zcts) offsetT = discountFactor offsetT (yieldAt zcts)

    --toDdiscountFactors (Interpolated zcts) offsetT = [discountFactor offsetT (SimpleSpotRate 0.0 0.0)]
    --toDdiscountFactors (Analytical zcts) offsetT = [discountFactor offsetT (SimpleSpotRate 0.0 0.0)]
