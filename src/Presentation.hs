-- Copyright:   (c) 2014 Andreas Bock, Johan Astborg
-- License:     BSD-3
-- Maintainers: Andreas Bock <bock@andreasbock.dk>
--              Johan Astborg <joastbg@gmail.com>
-- Stability:   experimental
-- Portability: portable
module Presentation where
import Utils.Calendar
import Utils.Currency
import Utils.DayCount
import Utils.Graphics.Visualize

import Instruments.Instrument

import Instruments.Utils.InterestRate
import Instruments.Utils.TermStructure

import Instruments.FixedIncome.Bonds.Bonds

--------------------------------------------------
-- NB: The tests here use your machine's system --
-- time and are therefore time sensitive!       --
--------------------------------------------------

-- Define values to be used when creating bonds 
settle   = read "2014-02-18" :: Date -- Settlement date
maturity = read "2016-02-18" :: Date -- Maturity of the bonds
face     = Cash (10^6) DKK           -- Principal
dayCount = ACTACT                    -- Daycount convention
rollConv = ModifiedFollowing         -- How to roll days
myRate   = 0.1                       -- Interest rate of 10%
stms     = 2                         -- Semiannual coupons

-- Examples of fixed income instruments
zero    = Zero    settle maturity face myRate      dayCount rollConv
bullet  = Bullet  settle maturity face myRate stms dayCount rollConv
consol  = Consol  settle          face myRate stms dayCount rollConv
serial  = Serial  settle maturity face myRate stms dayCount rollConv
annuity = Annuity settle maturity face myRate stms dayCount rollConv

-- We use an analytical term structure in this example:
ts = AnalyticalTermStructure $ \x -> (5 + (1/4)*sqrt x)/100

presentValueZero    = pv zero ts
presentValueBullet  = pv bullet ts
presentValueConsol  = pv consol ts
presentValueSerial  = pv serial ts
presentValueAnnuity = pv annuity ts
