{-# LANGUAGE TypeFamilies #-}
module Utils.DayCount where
import qualified Data.Time.Calendar as Cal
import qualified Data.Time as T
import Utils.Calendar

class DayCount d where
  modifier :: d -> Date -> Date -> Double

data Basis = ACTACT | ACT360   | ACT365F  | Thirty360
           | SIA    | Business | European | Japanese

instance DayCount Basis where
  modifier ACTACT dt0 dt1 = dc dt0 end0 + dc dt1 start1 + yearsBetween
    where dc start end 
            | isInLeapYear start = between / 366
            | otherwise          = between / 365
            where between = abs $ diffTime start end
          (y0,_,_) = Cal.toGregorian dt0
          (y1,_,_) = Cal.toGregorian dt1
          yearsBetween = fromIntegral $ y1-y0-1
          -- Construct border dates to compute offset
          end0   = Cal.fromGregorian (y0+1) 1 1
          start1 = Cal.fromGregorian y1 1 1
  modifier ACT360 d0 d1 = diffTime d1 d0 / 360
  modifier ACT365F d0 d1 = diffTime d1 d0 / 365
  modifier Thirty360 dt0 dt1 = dc0/360 + dc1/360 + yearsBetween
    where (y0,m0,d0) = Cal.toGregorian dt0
          (y1,m1,d1) = Cal.toGregorian dt1
          dc0 = fromIntegral $ 30*(12 - m0) + min 30 d0
          dc1 = fromIntegral $ 30*m1 + min 30 d1 - 1
          yearsBetween = fromIntegral $ y1-y0-1

-- Tests

-- Ex4 http://www.deltaquants.com/day-count-conventions.html
a = read "2008-02-01" :: Date
b = read "2009-05-31" :: Date

test0 = 1.3262594505576764 == modifier ACTACT a b
test1 = 1.3333333333333335 == modifier Thirty360 a b
test2 = 1.3472222222222223 == modifier ACT360 a b
test = all (==True) [test0, test1, test2]
