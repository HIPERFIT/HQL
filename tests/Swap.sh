let prin = Cash 10000000 DKK in 
let rate = SimpleRate 5.0 in
let bas = ACTACT in
let disc = 4.0 in
let dates = [fromGregorian 2010 03 03] in
let l1 = FixedLeg prin rate bas disc dates in l1