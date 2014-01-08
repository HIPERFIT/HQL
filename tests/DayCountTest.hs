import Utils.DayCount
import Utils.Calendar

-- Daycount convention tests
-- Factors taken from http://www.deltaquants.com/day-count-conventions.html

-- Example 1
ex1_d0 = read "2007-12-28" :: Date
ex1_d1 = read "2008-02-28" :: Date

ex1_1 = 0.16942884946478032 == modifier ACTACT ex1_d0 ex1_d1
ex1_2 = 0.16986301369863013 == modifier ACT365F ex1_d0 ex1_d1
ex1_3 = 0.16666666666666669 == modifier Thirty360 ex1_d0 ex1_d1
ex1_4 = 0.17222222222222222 == modifier ACT360 ex1_d0 ex1_d1
ex1   = all (==True) [ex1_1, ex1_2, ex1_3, ex1_4]

-- Example 2
ex2_d0 = read "2007-12-28" :: Date
ex2_d1 = read "2008-02-29" :: Date

ex2_1 = 0.1721610899019388  == modifier ACTACT ex2_d0 ex2_d1
ex2_2 = 0.1726027397260274  == modifier ACT365F ex2_d0 ex2_d1
ex2_3 = 0.16944444444444445 == modifier Thirty360 ex2_d0 ex2_d1
ex2_4 = 0.175               ==  modifier ACT360 ex2_d0 ex2_d1
ex2   = all (==True) [ex2_1, ex2_2, ex2_3, ex2_4]

-- Example 3
ex3_d0 = read "2007-10-31" :: Date
ex3_d1 = read "2008-11-30" :: Date

ex3_1 = 1.0824313197095592 == modifier ACTACT ex3_d0 ex3_d1
ex3_2 = 1.084931506849315  == modifier ACT365F ex3_d0 ex3_d1
ex3_3 = 1.0833333333333333 == modifier Thirty360 ex3_d0 ex3_d1
ex3_4 = 1.1                == modifier ACT360 ex3_d0 ex3_d1
ex3   = all (==True) [ex3_1, ex3_2, ex3_3, ex3_4]

-- Example 4
ex4_d0 = read "2008-02-01" :: Date
ex4_d1 = read "2009-05-31" :: Date

ex4_1 = 1.3262594505576764 == modifier ACTACT ex4_d0 ex4_d1
ex4_2 = 1.3287671232876712 == modifier ACT365F ex4_d0 ex4_d1
ex4_3 = 1.3333333333333333 == modifier Thirty360 ex4_d0 ex4_d1
ex4_4 = 1.3472222222222223 == modifier ACT360 ex4_d0 ex4_d1
ex4   = all (==True) [ex4_1, ex4_2, ex4_3, ex4_4]

allPassed = ex1 && ex2 && ex3 && ex4
