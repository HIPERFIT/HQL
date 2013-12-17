{- TODO: imports
> type InterestRate = Double
> df :: InterestRate -> Years -> Double
> df r yrs = 1.0 / (1.0 + (r / 100.0)) ** yrs

Take DiscountFunction, use currying from discount function
PartialDiscountFunction
> pv' :: (Years -> Double) -> Date -> Payment -> Double
> pv' df now (Payment date (Cash v _)) = (df yrs) * v
>   where yrs = (fromIntegral $ Calendar.diffDays date now) / 365.0 -- TODO: leap year
 
> yrs' now date = (fromIntegral $ Calendar.diffDays date now) / 365.0
 
> rate1 = Flat 12
> now  = (read "2011-01-01") :: Date
> d1   = (read "2011-12-31") :: Date -- 1 yrs from spot
> d2   = (read "2012-12-31") :: Date -- 2 yrs from spot
> d3   = (read "2013-12-31") :: Date -- 3 yrs from spot
> d4   = (read "2014-12-31") :: Date -- 4 yrs from spot
> d5   = (read "2015-12-31") :: Date -- 5 yrs from spot
> d6   = (read "2016-12-31") :: Date -- 6 yrs from spot
> d10  = (read "2020-12-31") :: Date -- 10 yrs from spot
> c0 = Cash 100.0 USD
> c1 = Cash 1000.0 USD
 
> pm1 = Payment d5 c0
> pms = [Payment d5 c0, Payment d6 c0]
> zpm = Payment d10 $ Cash 1000.0 USD
 
Present value of 100 received in 5 years, 12.0% interest annualy
> pv (df rate1) now pm1
56.742685571859916

Two payments discounted 12.0% interest rate
> map (pv (df rate1) now) pms
[56.742685571859916,50.64738419271504]

Sum of discounted cashflows
> sum $ map (pv (df rate1) now) pms
107.39006976457495

Model a Zero-Coupon Bond 
3% coupon rate, 10 years to maturity, 1000 in face
> zz = pv' (df (Flat 3)) now zpm 
743.9734067115595

Model an Annuity
5% coupon rate, 5 years to maturity, coupons of 1000 each
face of 5000, coupon of 20%
> apms = [Payment a b | a <- [d1, d2, d3, d4, d5], b <- [c1]]
> z    = sum $ map (pv' (df (Flat 5.0)) now) apms
4329.476670630818
-}

{- Sample usage - (old Bonds2.hs)

    > let pm1 = Payment 5.0 100.0
    > let pms = [Payment 5.0 100.0, Payment 6.0 100.0]
    
    -- Present value of 100 received in 5 years, 12.0% interest annualy
    > pv (df 12.0) pm1
    56.742685571859916
    
    -- Two payments discounted 12.0% interest rate
    > map (pv $ df 12.0) pms
    [56.742685571859916,50.663112117732055]
    
    -- Sum of discounted cashflows
    > sum $ map (pv $ df 12.0) pms
    107.40579768959196

    -- Model a Zero-Coupon Bond 
    -- 3% coupon rate, 10 years to maturity, 1000 in face
    > zpms = [Payment 10.0 1000.0]
    > sum $ map (pv $ df 3.0) zpms
    744.093914896725

    -- Model an Aunnuity
    -- 5% coupon rate, 5 years to maturity, coupons of 1000 each
    -- face of 5000, coupon of 20%
    > let apms = [Payment a b | a <- [1..5], b <- [1000.0]]
    > sum $ map (pv $ df 5.0) apms
    4329.476670630818
    
    -- We can model each Bond using these payments
    -- Need to support termstructure in df / DiscountFunction
-}
