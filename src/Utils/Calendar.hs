module Calendar where
import qualified Data.Time as T
import qualified Data.Time.Calendar.WeekDate as WeekDate
import qualified Data.Time.Calendar as Cal

type Date = T.Day
type Days = Integer
type Years = Double

data RollConvention = Following | Preceding | ModifiedFollowing
type Settlements = Int

-- This uses interpolation
getSettlementDates :: RollConvention -> Settlements -> Date -> [Date]
getSettlementDates conv sts dt = map (rollDay conv) $ iterate nextDate dt
 where nextDate = T.addDays daysBetween
       daysBetween   = daysBetweenSettlements dt sts

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
