-- Module:      HQL
-- Copyright:   (c) 2013 HIPERFIT
-- License:     BSD-3
-- Maintainers: Andreas Bock <bock@andreasbock.dk>
--              Johan Astborg <joastbg@gmail.com>
-- Stability:   experimental
-- Portability: portable
--

import Utils.Calendar
import Utils.Currency
import Utils.DayCount

import Instruments.Instrument

import Instruments.Utils.InterestRate
import Instruments.Utils.TermStructure

import Instruments.FixedIncome.Bonds.Bonds


--
-- Tests
--
settle = (read "2010-01-01")::Date 
-- maturity = (read "2014-07-02")::Date
maturity = (read "2016-01-01")::Date
maturity1 = (read "2015-01-02")::Date
maturity2 = (read "2013-01-01")::Date
rate1 = 0.1
rate2 = 0.1
stms1 = 1 :: Settlements
stms2 = 2 :: Settlements
present = settle

-- Example instruments
zero    = Zero settle maturity (Cash 100 SEK) rate1 ACTACT ModifiedFollowing
bullet  = Bullet settle maturity1 (Cash 100 USD) rate1 stms2 ACTACT Following
consol  = Consol settle (Cash 100 USD) rate1 stms2 ACTACT Following
serial  = Serial settle maturity (Cash 100 USD) rate1 stms1 ACTACT Following
annuity = Annuity settle maturity2 (Cash 100 GBP) rate2 stms2 ACTACT ModifiedFollowing

---------------
---- TESTS ----
---------------
tz0 = \x -> (5 + (1/4)*sqrt x)/100
ts0 = AnalyticalTermStructure tz0
s0  = (read "2015-01-01")::Date 
m0  = (read "2022-07-01")::Date
r0  = 0.07
stms0 = 2
-- Zero
z0     = Zero s0 m0 (Cash 147 SEK) r0 ACTACT ModifiedFollowing
cf_z0  = cashflow z0
cps_z0 = coupons z0
pv_z0  = pv z0 ts0
-- Annuity
a0     = Annuity s0 m0 (Cash 100 GBP) r0 stms0 ACTACT ModifiedFollowing
cf_a0  = cashflow a0
cps_a0 = coupons a0
pv_a0  = pv a0 ts0   -- FAILS
-- Bullet
b0     = Bullet s0 m0 (Cash 100 USD) r0 stms0 ACTACT Following
cf_b0  = cashflow b0
cps_b0 = coupons b0
pv_b0  = pv b0 ts0
-- Consol
c0     = Consol s0 (Cash 100 USD) r0 stms0 ACTACT Following
cf_c0  = cashflow c0
cps_c0 = coupons c0
pv_c0  = pv c0 ts0
-- Serial
sr0     = Serial s0 m0 (Cash 100 USD) r0 stms0 ACTACT Preceding
cf_sr0  = cashflow sr0
cps_sr0 = coupons sr0
pv_sr0  = pv sr0 ts0 -- FAILS
