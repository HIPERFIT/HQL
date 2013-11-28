--
-- Types
--

--- Support for currency
{-
data Currency = USD | EUR | DKK | SEK deriving Show

data Cash = Cash Double Currency deriving Show

data CurrencyPair = CurrencyPair Currency Currency deriving Show

data ExchangeRate = ExchangeRate Double CurrencyPair deriving Show
-}

type Date = Double -- Hack to test out implementation, will be date(s)

type CouponRates = [Double]

-- Some common currencies
data Currency = USD | EUR | GBP | CHF | JPY | DKK | SEK deriving Show

data Cash = Cash Double Currency | InvalidCash
  
-- Format cash using currency symbol
-- TODO: Format with ***##.## decimals etc
instance Show Cash where
    show (Cash v USD) = "$" ++ show v
    show (Cash v EUR) = "€" ++ show v
    show (Cash v GBP) = "£" ++ show v
    show (Cash v JPY) = "¥" ++ show v
    show (Cash v CHF) = show v ++ " CHF"
    show (Cash v DKK) = show v ++ " kr"
    show (Cash v SEK) = show v ++ " kr"
    show InvalidCash = "InvalidCash"

instance Num Cash where
    (Cash v USD) + (Cash w USD) = Cash (v + w) USD
    (Cash v EUR) + (Cash w EUR) = Cash (v + w) EUR
    (Cash v GBP) + (Cash w GBP) = Cash (v + w) GBP
    (Cash v CHF) + (Cash w CHF) = Cash (v + w) CHF
    (Cash v JPY) + (Cash w JPY) = Cash (v + w) JPY
    (Cash v DKK) + (Cash w DKK) = Cash (v + w) DKK
    (Cash v SEK) + (Cash w SEK) = Cash (v + w) SEK
    _ + _ = InvalidCash
    
    (Cash v USD) * (Cash w USD) = Cash (v * w) USD
    (Cash v EUR) * (Cash w EUR) = Cash (v * w) EUR
    (Cash v GBP) * (Cash w GBP) = Cash (v * w) GBP
    (Cash v CHF) * (Cash w CHF) = Cash (v * w) CHF
    (Cash v JPY) * (Cash w JPY) = Cash (v * w) JPY
    (Cash v DKK) * (Cash w DKK) = Cash (v * w) DKK
    (Cash v SEK) * (Cash w SEK) = Cash (v * w) SEK
    _ * _ = InvalidCash

    -- TODOs: Ajust for negative cash?
    fromInteger v = InvalidCash
    abs c = c
    signum c = c
        

data CurrencyPair = CurrencyPair Currency Currency

instance Show CurrencyPair where
    show (CurrencyPair b q) = show b ++ "/" ++ show q

data ExchangeRate = ExchangeRate Double CurrencyPair

-- TODO: Bid/Ask
instance Show ExchangeRate where
    show (ExchangeRate mid pair) = show pair ++ " " ++ show mid 

{-

    For example, use the Cash data
    > Cash 115.2 USD
    $115.2

    > Cash 89.0 SEK
    89.0 kr

    Define a currecy pair
    > CurrencyPair EUR USD
    EUR/USD

    Specify an exchange rate for EUR/USD
    > ExchangeRate 1.32 (CurrencyPair EUR USD)
    EUR/USD 1.21

    **** TODOs
    Support conversion of currencies, like this:
    
    Cash 1000.00 USD `to` EUR
    -- Will use implicits and infix operator
    -- Idea: use the exchange rate as an implicit (ImplicitParams)
    -- See: http://www.haskell.org/haskellwiki/Implicit_parameters

-}

--type Years = Double -- Can be fractional
--type Payment = Cash Double
--type Payments = M.Map Date Payment
--type Settlements = Int

--instance Show Payment where
--    show a b = (show a) ++ " " ++ (show b)

type IssueDate = Date
type Settlements = [Date]

data Payment = Payment Date Double deriving Show
type Payments = [Payment]

-- DiscountFunction and PV

--data DiscountFunction 
-- Interest rate to come from term structure

-- Takes interest rate in percent -- TODO: Type for this, prevent errors
-- Take InterestRate Years
df :: Double -> Double -> Double
df = (\r n -> 1.0 / (1.0 + (r / 100.0)) ** n)

-- Take DiscountFunction, use currying from discount function
-- PartialDiscountFunction
pv :: (Double -> Double) -> Payment -> Double
pv = (\df (Payment y a) -> (df y) * a)

{- Sample usage

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

-- pv $ df ts payments

-- pv df $ payments

-- Present value
--presentValue p = 

---- Bond
---- Common for all bonds

-- Day-count basis
data Basis = ActualActual | SIA | Business | European | Japanese

data Compounding = Continous | Periodic Int

-- End-of-month rule
data EndMonthRule = Ignore | Apply

data Bond = Bond {
    issueDate    :: IssueDate,
    settlements  :: Settlements,   -- Settlement date
    maturity     :: Date,          -- Maturity date
    couponRates  :: CouponRates,   -- Coupon rate
    compounding  :: Compounding,           -- Coupons per year (default = 2)
    basis        :: Basis,         -- Day-count basis
    endMonthRule :: EndMonthRule,  -- End-of-month rule
    face         :: Double         -- Face value of bond
}

{-
    **** Notes
    
    Every bond will produce a series of payments, depending on settings
    
    DiscountFunction will be depending on the period
    
    TermStructure will determine the interest rate at each point in time
    
    ** Easy to extend to use dates etc, and amortize for passed interest coupons
    
    ** Able to use PV, FV and Value(t) using this method

-}
