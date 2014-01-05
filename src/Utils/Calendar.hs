module Utils.Calendar where

import qualified Data.Time as T
import qualified Data.Time.Calendar.WeekDate as WeekDate
import qualified Data.Time.Calendar as Cal

type Date = T.Day
type Days = Integer
type Years = Double

data RollConvention = Following | Preceding | ModifiedFollowing
type Settlements = Int

data Basis = ACTACT | ACT360 | ACT365F | Thirty360
           | SIA | Business | European | Japanese
data EndMonthRule = Ignore | Apply

class Calendar c where
  isLegalDay :: c -> Date -> Bool
  isHoliday  :: c -> Date -> Bool

-- TODO: refactor extrapolate/interpolate
-- This uses interpolation 
extrapolateDates :: RollConvention -> Settlements -> Date -> [Date]
extrapolateDates conv stms from = map (rollDay conv) $ iterate (T.addDays between) from
  where  between = daysBetweenSettlements from stms

interpolateDates :: Date -> RollConvention -> Settlements -> Date -> [Date]
interpolateDates mat conv stms from = iterateEnd from
  where  between = daysBetweenSettlements from stms
         iterateEnd date
           | T.diffDays mat date' < 0 = mat : []
           | otherwise = date' : iterateEnd date'
           where date' = rollDay conv $ T.addDays between date

ost conv between date = rollDay conv $ T.addDays between date

{-
-- Tests
d1 = (read "2001-01-01")::Date
d2 = (read "2006-01-01")::Date
interpolateDates d2 Following (Periodic 1) d1 
> [2002-01-01,2003-01-01,2004-01-01,2004-12-31,2006-01-02]
-}

daysBetweenSettlements :: Date -> Settlements -> Days
daysBetweenSettlements d sts 
  | T.isLeapYear y = floor $ 366.0 / fromIntegral sts -- floor or ceil?
  | otherwise  = floor $ 365.0 / fromIntegral sts
  where (y,_,_) = WeekDate.toWeekDate d

rollDay :: RollConvention -> Date -> Date
rollDay conv date
  | 1 <= day && day <= 5 = date
  | otherwise = doRoll conv date
  where (_,_,day) = WeekDate.toWeekDate date

doRoll :: RollConvention -> Date -> Date
doRoll Following date = rollForward date
doRoll Preceding date = rollBackwards date
doRoll ModifiedFollowing date
  | mn' == mn = fwdDate
  | otherwise = rollBackwards date
  where (_, mn', _) = Cal.toGregorian fwdDate
        (_, mn , _) = Cal.toGregorian date
        fwdDate = rollForward date

legalDay :: Date -> Bool -- TODO: Add argument so users may specify holidays 
legalDay date
  | 1 <= day && day <= 5 = True
  | otherwise = False
  where (_,_, day) = WeekDate.toWeekDate date

rollForward, rollBackwards :: Date -> Date
rollForward date
  | legalDay date = date
  | otherwise     = rollForward $ Cal.addDays 1 date
rollBackwards date
  | legalDay date = date
  | otherwise     = rollBackwards $ Cal.addDays (-1) date

getYearOffset :: Date ->  Date -> Years
getYearOffset now date
  | Cal.isLeapYear y = diff / 366
  | otherwise      = diff / 365
  where (y,_,_) = WeekDate.toWeekDate date
        diff = fromIntegral $ Cal.diffDays date now 

getDay :: IO Date
getDay = T.getCurrentTime >>= \d -> return (T.utctDay d)

isExpired maturityDate = do {now <- getDay; return $ T.diffDays now maturityDate < 0}
