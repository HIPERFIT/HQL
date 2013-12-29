-- Module for Interest rates
module Interest where
import Calendar
import Text.Printf

type Rate = Double
data InterestRate = InterestRate Compounding Rate

instance Show InterestRate where
  show = printf "InterestRate %.2f%%" . interestRate

-- instance Show InterestRate where
--   show (InterestRate r) = printf "InterestRate %.2f%%" r

-- |Returns a continuously compounded interest rate
interestRate :: InterestRate -> Rate 
interestRate (InterestRate Continuous rate) = rate
interestRate (InterestRate (Periodic p) rate) = log(1.0 + rate/100.0)*100.0

-- |Returns a continuously compounded interest rate
-- interestRate :: Compounding -> Rate -> InterestRate
-- interestRate Continuous rate = InterestRate rate
-- interestRate (Periodic p) rate = interestRate Continuous (log(1.0 + rate/100.0)*100.0)
