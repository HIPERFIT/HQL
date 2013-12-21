module Discounting where
import Prelude
import Data.Char

--import Bonds
--import TermStructure
--import Currency
--import Calendar
--type DiscountFactor = Double

-- Interval for periodic compounding, day-count conventions etc
data Interval = Annually | SemiAnnually | Quarterly | Monthly | Other Double deriving (Show)

-- The compounding can be either continous or periodic
data Compounding = Continuous | Periodic Interval deriving (Show)

-- The interest rate is described by the compounding and rate
type Rate = Double
data InterestRate = InterestRate Compounding Rate

instance Show InterestRate where
  show (InterestRate c r) = show r ++ "% " ++ (map toLower (show c)) ++ " compounded"

-- Continuous compounding factor p(t,T)
-- Zero coupon bond with nominal value N=1
--ContinuousDiscountFactor :: Double -> Double -> Double
--ContinuousDiscountFactor = 0

-- DiscountFactor is a curried function from [t,S,T]
-- where the future times S < T are prevailing at time t
-- Present value is represented as [0,0,T] (t=S)
-- Future value at time t=0 as [0,S,T]
--DiscountFactor :: Double -> Double -> Double -> Double
--discountFactor ir tt tT = exp(-(ir/100)*(tT - tt))

-- TODO: Only use continous compound factor

discountFactor (InterestRate Continuous r) tT tt = exp(-(r/100)*(tT - tt))
discountFactor (InterestRate (Periodic p) r) tT tt = case p of
    Annually        -> ccf r tT 1
    SemiAnnually    -> ccf r tT 2
    Quarterly       -> ccf r tT 4
    Monthly         -> ccf r tT 12
    Other i         -> ccf r tT i
    where ccf r t p = 1/(1 + (r/100)/p)**(p*t)
    --where ccf r t p = 1/(1 + (r/100)/p)**(p*t)
--    exp(-(r/100)*(tT - tt))

-- Term structure - yield curve
-- analytic | from data
termStructure x = 5 + (1/2)*sqrt(x)


-- TESTS (1-17 non-calendar time)

{-
NTest[(* 5 *)
b = N[DiscountFactor[1, 9 + 28/100, Compounding -> {Linear, 1}, CompiledDF -> True], 50]
,
0.9150805270863837
]
## HQL
discountFactor (InterestRate (Periodic Annually) (9+28/100 :: Double)) 1.00 0.0 == 0.9150805270863837
-}


{-
NTest[(* 7 *)
DiscountFactor[1.5, termstructure1, Compounding -> {Linear, 2}]
,
0.9203271932613013
]
## HQL (using term structure)
> let termStructure x = 5 + (1/2)*sqrt(x)
> let r = (termStructure 1.5)
> discountFactor (InterestRate (Periodic SemiAnnually) r) 1.50 0.0 == 0.9203271932613013
-- refined version
-}


{-
NTest[(* 8 *)
DiscountFactor[1.5, termstructure1, Compounding -> {Linear, 2}, CompiledDF -> True]
,
0.9203271932613013
]
## HQL
> discountFactor (InterestRate (Periodic SemiAnnually) (termStructure 1.5)) 1.5 0.0 == 0.9203271932613013
-}


{-
NTest[(* 9 *)
DiscountFactor[3, 8.5, Compounding -> {Exponential, 12}]
,
0.7756133702070986
]
## HQL
> discountFactor (InterestRate (Periodic Monthly) 8.5) 3.0 0.0 == 0.7756133702070988
-}

{-
NTest[(* 10 *)
DiscountFactor[3, 8.5, Compounding -> {Exponential, 12}, CompiledDF -> True]
,
0.7756133702070988
]
## HQL
> discountFactor (InterestRate (Periodic Monthly) 8.5) 3.0 0.0 == 0.7756133702070988
-}

{-
NTest[(* 16 *)
DiscountFactor[1/2, 8.3, Compounding -> Continuous, CompiledDF -> True]
,
0.9593493353414723
]
## HQL
> discountFactor (InterestRate Continuous 8.3) 0.5 0 == 0.9593493353414723
-}


{-
NTest[(* 11 *)
DiscountFactor[4.5, termstructure1, Compounding -> {Exponential, 2}]
,
0.7643885607510086
]
## HQL
> let termStructure x = 5 + (1/2)*sqrt(x)
> let r = (termStructure 4.5)
> discountFactor (InterestRate (Periodic SemiAnnually) r) 4.50 0.0 == 0.7643885607510086
-}


{-
NTest[(* 12 *)
DiscountFactor[4.5, termstructure1, Compounding -> {Exponential, 2}, CompiledDF -> True]
,
0.7643885607510086
]
## HQL
> discountFactor (InterestRate (Periodic SemiAnnually) (termStructure 4.5)) 4.5 0.0 == 0.7643885607510086
-}


{-
NTest[(* 13 *)
DiscountFactor[1/2, 8.5, Compounding -> {LinearExponential, 1}]
,
0.9592326139088729
]
## HQL (convert periodic to continous)
> discountFactor (InterestRate (Periodic SemiAnnually) 8.5) 0.5 0.0 == 0.9592326139088729
-}


{-
NTest[(* 14 *)
DiscountFactor[1/2, 8.5, Compounding -> {LinearExponential, 1}, CompiledDF -> True]
,
0.9592326139088729
]
## HQL
> discountFactor (InterestRate (Periodic SemiAnnually) 8.5) 0.5 0.0 == 0.9592326139088729
-}


{-
NTest[(* 15 *)
DiscountFactor[1/2, 8.3, Compounding -> Continuous]
,
0.9593493353414723
]
## HQL
> discountFactor (InterestRate Continuous 8.3) 0.5 0.0 == 0.9593493353414723
-}


{-
NTest[(* 16 *)
DiscountFactor[1/2, 8.3, Compounding -> Continuous, CompiledDF -> True]
,
0.9593493353414723
]
## HQL
> discountFactor (InterestRate Continuous 8.3) 0.5 0.0 == 0.9593493353414723
-}


{-
NTest[(* 17 *)
N[DiscountFactor[90/360, termstructure1, Compounding -> Continuous]]
,
0.9871663954590084
]
## HQL
> discountFactor (InterestRate Continuous (termStructure (90/360 :: Double))) (90/360 :: Double) 0.0 == 0.9869607572146836
-}


---- FORWARD RATE
{-
n = 2;
theta1 = 0.25;
theta2 = 1.5;
interest1 = 6.65;
interest2 = 7.77;
Out[8]= 7.99473

## HQL
> (((discountFactor (InterestRate (Periodic SemiAnnually) 6.65) 0.25 0.0)/(discountFactor (InterestRate (Periodic SemiAnnually) 7.77) 1.5 0.0))**(1/(2*1.25))-1)*2*100 == 7.994727369824384

-}


-- Include in tests error: abs(output - expected) / expected

-- Create a new discount factor from input, where several
-- ways are supported. Create DiscountFactor from:
-- 1) InterestRate
-- 2) TermStructure (yield curve)
-- 3) Forward rates (has to be two)

{-
newDF :: InterestRate -> DiscountFactor


    Create discount factor (ie zero bond)
    - Interest rate (as percentage)
    - Time to maturity
    - Linear, Exponential, Continous => Periodic, Continuous



-- Computes the discount factor
df :: InterestRate -> Years -> DiscountFactor
df = \r n -> 1.0 / (1.0 + (r / 100.0)) ** n

-- Computes a list of discount factors
dfs :: TermStructure -> [Years] -> [DiscountFactor]
dfs (Flat r) offsets = map (df r) offsets
dfs (Analytical _) _ = undefined
dfs (Interpolated ts) offsets = zipWith df ts offsets -- TODO: match on dates?

discountPayment :: Date -> InterestRate -> Payment -> Cash
discountPayment now r (Payment date cash) = scale (df r y) cash
  where y = getDayOffset now date

discountPayments :: Date -> TermStructure -> Payments -> [Cash]
discountPayments now ts pms = zipWith (discountPayment now) myDfs pms
  where offsets = map (\(Payment d _) -> getDayOffset now d) pms
        myDfs = dfs ts offsets
        
-}

