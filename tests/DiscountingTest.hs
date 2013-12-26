-- TESTS (1-17 non-calendar time)

{-
NTest[(* 5 *)
b = N[DiscountFactor[1, 9 + 28/100, Compounding -> {Linear, 1}, CompiledDF -> True], 50]
,
0.9150805270863837
]
## HQL
discountFactor (InterestRate (Periodic 1) (9+28/100 :: Double)) 1.00 0.0 == 0.9150805270863837
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
> discountFactor (InterestRate (Periodic 2) r) 1.50 0.0 == 0.9203271932613013
-- refined version
-}


{-
NTest[(* 8 *)
DiscountFactor[1.5, termstructure1, Compounding -> {Linear, 2}, CompiledDF -> True]
,
0.9203271932613013
]
## HQL
> discountFactor (InterestRate (Periodic 2) (termStructure 1.5)) 1.5 0.0 == 0.9203271932613013
-}


{-
NTest[(* 9 *)
DiscountFactor[3, 8.5, Compounding -> {Exponential, 12}]
,
0.7756133702070986
]
## HQL
> discountFactor (InterestRate (Periodic 12) 8.5) 3.0 0.0 == 0.7756133702070988
-}

{-
NTest[(* 10 *)
DiscountFactor[3, 8.5, Compounding -> {Exponential, 12}, CompiledDF -> True]
,
0.7756133702070988
]
## HQL
> discountFactor (InterestRate (Periodic 12) 8.5) 3.0 0.0 == 0.7756133702070988
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
> discountFactor (InterestRate (Periodic 2) r) 4.50 0.0 == 0.7643885607510086
-}


{-
NTest[(* 12 *)
DiscountFactor[4.5, termstructure1, Compounding -> {Exponential, 2}, CompiledDF -> True]
,
0.7643885607510086
]
## HQL
> discountFactor (InterestRate (Periodic 2) (termStructure 4.5)) 4.5 0.0 == 0.7643885607510086
-}


{-
NTest[(* 13 *)
DiscountFactor[1/2, 8.5, Compounding -> {LinearExponential, 1}]
,
0.9592326139088729
]
## HQL (convert periodic to continous)
> discountFactor (InterestRate (Periodic 2) 8.5) 0.5 0.0 == 0.9592326139088729
-}


{-
NTest[(* 14 *)
DiscountFactor[1/2, 8.5, Compounding -> {LinearExponential, 1}, CompiledDF -> True]
,
0.9592326139088729
]
## HQL
> discountFactor (InterestRate (Periodic 2) 8.5) 0.5 0.0 == 0.9592326139088729
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
> (((discountFactor (InterestRate (Periodic 2) 6.65) 0.25 0.0)/(discountFactor (InterestRate (Periodic 2) 7.77) 1.5))**(1/(2*1.25))-1)*2*100 == 7.994727369824384

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
       
-}
