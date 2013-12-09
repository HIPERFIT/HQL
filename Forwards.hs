import Calendar
import Currency
import Bonds
data Forward = Forward {
    bondUnderlying :: Cashflow,
    offset         :: Date
    }

forward :: Cashflow -> Date -> Forward
forward = undefined
