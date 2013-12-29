-- Module for Interest rates
module Interest where
import Text.Printf

data Compounding = Continuous | Periodic Int deriving (Show)

type Rate = Double
data InterestRate = InterestRate Rate

instance Show InterestRate where
  show (InterestRate r) = printf "InterestRate %.2f%%" r

-- |Returns a continuously compounded interest rate
interestRate :: Compounding -> Rate -> InterestRate
interestRate Continuous rate = InterestRate rate
interestRate (Periodic p) rate = interestRate Continuous (log(1.0 + rate/100.0)*100.0)
