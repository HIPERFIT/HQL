module Instruments.Utils.InterestRate where
 
type Rate = Double
type DayCounter = Double

data Frequency = Once -- For Zero Bond
               | Annually
               | SemiAnnually
               | Quarterly
               | Monthly
	       | NoFrequency 
               | OtherFrequency Int deriving (Show)

data Compounding = Continuous 
		 | Compounded 
		 | Simple 
		 | SimpleThenCompounded deriving (Show)
 
data ContinuousInterestRate = ContinuousInterestRate Rate deriving (Show)
data CompoundedInterestRate = InterestRate Rate Compounding Frequency deriving (Show)
 
class InterestRate a where
    rate 	:: a -> Rate
    compounding :: a -> Compounding
    frequency   :: a -> Frequency
    toContinuous :: a -> ContinuousInterestRate 
 
instance InterestRate ContinuousInterestRate where
    rate (ContinuousInterestRate r)	= r
    compounding                         = const Continuous
    frequency                     	= const Annually
    toContinuous (ContinuousInterestRate r) = ContinuousInterestRate r

instance InterestRate CompoundedInterestRate where
    rate (InterestRate r _ _ ) 	= r
    compounding (InterestRate _ c _) = c
    frequency (InterestRate _ _ f)   = f
    toContinuous (InterestRate r Continuous _) = ContinuousInterestRate r
    toContinuous (InterestRate r c f) = case f of
        Once             -> ContinuousInterestRate (mc r 1)
	Annually         -> ContinuousInterestRate (mc r 1)
	SemiAnnually     -> ContinuousInterestRate (mc r 2)
  	Quarterly        -> ContinuousInterestRate (mc r 4)
  	Monthly          -> ContinuousInterestRate (mc r 12)
   	OtherFrequency i -> ContinuousInterestRate (mc r (fromIntegral i))
   	_ -> ContinuousInterestRate 0.0 
	where mc r f = f*(exp(r/(100*f)) - 1)*100


-- Helpers
--mc 1 r = log(1+r/100.0)*100.0
--mc p r = (log((1.0+r/(100.0*pp))**pp))*100 where pp = fromIntegral p
