-- |
-- Module:      Utils.DayCount
-- Copyright:   (c) Johan Astborg, Andreas Bock
-- License:     BSD-3
-- Maintainer:  Andreas Bock <bock@andreasbock.dk>
-- Stability:   experimental
-- Portability: portable
--
-- Defines types and classes for computing day count modifiers
module Utils.DayCount where
import qualified Data.Time.Calendar as Cal
import qualified Data.Time as T
import Utils.Calendar

data Basis = ACTACT | ACT360 | ACT365F  | Thirty360 deriving (Show)

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
        dc0 = fromIntegral $ 30*(12 - m0) + 30 - min 30 d0 + leapFactor
        dc1 = fromIntegral $ 30*m1 - 30 + min 30 d1
        yearsBetween = fromIntegral $ y1-y0-1
        daysInMonth_d0 = Cal.gregorianMonthLength y0 m0
        daysInMonth_d1 = Cal.gregorianMonthLength y1 m1
        leapFactor
          | T.isLeapYear y0 = 1
          | otherwise       = 0
