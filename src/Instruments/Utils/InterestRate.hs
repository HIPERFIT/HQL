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

type ContinuousRate = Double
type CompoundedRate = Double
type Rate = Double
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
data Compounding = Continuous 
		 | Compounded 
		 | Simple 
		 | SimpleThenCompounded deriving (Show)

-- | The continous compounded interest rate is used internally
--
-- Every interest rate other than continuous is converted using toContinuous.
--data ContinuousInterestRate = ContinuousInterestRate Rate deriving (Show)
--data CompoundedInterestRate = InterestRate Rate Compounding Frequency deriving (Show)

--type Rate = Double
type Time = Double
type Term = Double
type Tenor = Double
type Maturity = Double

data DiscountFactor = DiscountFactor Rate Maturity deriving (Show)

--InterestRateInstrument = ForwardRate | InterestRate | TermStructure

data InterestRate = ShortRate Rate
		  | ContinuousSpotRate Rate Tenor 
	          | ContinuousForwardRate Rate Term Tenor
		  | LIBORSpotRate Rate Tenor 
	          | LIBORForwardRate Rate Term Tenor
	          deriving (Show)

{-
data ContinousInterestRate = ContinuousSpotRate Offset Offset 
			   | ContinuousForwardRate Time Offset Offset

data SimpleInterestRate = SimpleSpotRate Offset Offset 
			| SimpleForwardRate Time Offset Offset
-}

class InterestRateInstrument a where
    rate :: a -> Rate
    discountFactor :: a -> Time -> DiscountFactor

instance InterestRateInstrument InterestRate where

    -- Returns the short rate
    rate (ShortRate r) = r

    -- Returns the continuously compounded spot rate
    rate (ContinuousSpotRate r _) = r

    -- Returns the continuously compounded forward rate
    rate (ContinuousForwardRate r _ _) = r

    -- Returns the simple LIBOR spot rate
    rate (LIBORSpotRate r _) = r

    -- Returns the simple LIBOR forward rate
    rate (LIBORForwardRate r _ _) = r

    -- Convert interest rate to DiscountFactor at time t
    discountFactor (ShortRate r) t = DiscountFactor (exp(-(r/100.0)*t)) 0
	
    -- Returns the discount factor p(S,T)
    discountFactor (ContinuousSpotRate r termN) t = DiscountFactor (exp(-(r/100.0)*t)) termN

    -- Returns the continuously compounded forward rate for [S,T] contracted at t
    discountFactor (ContinuousForwardRate r termN tenorM) t = DiscountFactor (exp(-(r/100.0)*t)) tenorM

    -- Returns the LIBOR spot rate at time t
    discountFactor (LIBORSpotRate r termN) t = DiscountFactor (exp(-(r/100.0)*t)) termN

    -- Returns the LIBOR forward rate at time t
    discountFactor (LIBORForwardRate r termN tenorM) t = DiscountFactor (exp(-(r/100.0)*t)) tenorM

-- TODO: Test cases

{-

LIBORForwardRate (5.0 :: Rate) (6 :: Term) (9 :: Tenor)

 -
ShortRate rate
SpotRate offsetS offsetT DiscountFactor = 
ForwardRate t offsetS offsetT

ShortRate rate








Simple | Continous | Exponential 
-}

-- $use
-- 
-- This section contains basic information about using the
-- interest rate defined in this module.
--
-- To create a continuously compounded rate from a annually
-- discretely compounded rate of 5.0%:
--
-- > rate (toContinuous (InterestRate 5.0 Compounded Annually))
--
-- And another variation of the same theme:
--
-- > rate (toContinuous (InterestRate 10.4713 Compounded Monthly))

