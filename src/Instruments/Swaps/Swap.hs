{-# LANGUAGE TypeFamilies, GADTs, RecordWildCards, RankNTypes, MultiParamTypeClasses #-}
-- Module:      Instruments.Swap.Swap
-- Copyright:   (c) Kasper Passov
-- Maintainer:  Kasper Passov <kasper.passov@gmail.com>
--
-- Types and functions for working with and evaluating swaps

module Instruments.Swaps.Swap where

import Control.Monad (liftM)
import Data.List
import Data.Maybe(fromJust)
import Instruments.Utils.InterestRate
import Instruments.Utils.FloatingRate
import Instruments.Utils.TermStructure
import Utils.Currency as Cur
import Utils.Calendar
import Utils.DayCount
import Instruments.Instrument

    
-- | the spread in bas points of the floating leg
type Spread = Double
type Nominal = Double
type Discount = Double 
type Seed = Double
    
data Leg a where 
    FiLeg :: { prin :: Cash,
               dats :: [Date], 
               inra :: a,
               accr :: Basis} -> Leg a
    FlLeg :: { prin :: Cash,
               dats :: [Date],
               accr :: Basis} -> Leg a 
               
data VanillaSwap a where
     VanillaSwap :: {l1 :: Leg a, 
                     l2 :: Leg a} -> VanillaSwap a

class Instrument s => Swap s where
    --disFac :: s -> TermStructure -> DiscountFactor

instance InterestRate a => Swap (VanillaSwap a) where
    --npv leg1 leg2 = npv_leg leg1 - npv_leg leg2

instance InterestRate a => Instrument (VanillaSwap a) where
    type PricingEngine = TermStructure 
    expired VanillaSwap{..} = legExpired l1 l2
    pv VanillaSwap{..} ts = do{v1 <- pvLeg l1 ts;
                               v2 <- pvLeg l2 ts;
                               return $ v1 - v2}
            
pvLeg :: InterestRate a => Leg a -> TermStructure -> IO Cash
pvLeg FlLeg{..} ts = do{dates <- dateTrips;
                         return $ Cur.sum $ map (\(dt0, dt1, dt2) ->
                             let accrfacprev = modifier accr dt0 dt1 in -- The accrualfactor of calculation i - 1 in fraction of years
                             let accrfac = modifier accr dt1 dt2 in -- Accrualfactor for i in fraction of years
                             Cur.scale (fromJust (dfAt ts accrfacprev) - fromJust (dfAt ts accrfac)) prin) dates}
        where dateTrips = liftM getTrips $ getValid dats -- the dates (including today) in tuples of three.
        
pvLeg FiLeg{..} _ = do{dates <- datePairs;
                        return $ Cur.sum $ map (\(dt0,dt1) -> 
                            let accrfact = modifier accr dt0 dt1 in 
                                Cur.scale (accrfact * discountFactor inra accrfact) interest) 
                        dates}
        where interest = Cur.scale (rate inra / 100) prin
              datePairs = liftM getPairs $ getValid dats -- the dates (including today) in tuples of two

getValid :: [Date] -> IO [Date]                
getValid dats = do{today <- getDay;
                   valids <- filterExpired dats;
                   return (sort $ today : valids)} 
                        
getPairs :: [a] -> [(a, a)]
getPairs [] = []
getPairs [x] = []
getPairs (x:yz@(y:z)) = (x, y) : getPairs yz

getTrips :: [a] -> [(a, a, a)]
getTrips [] = []
getTrips [x] = []
getTrips (x:y:[]) = [] 
getTrips (x:yzv@(y:z:v)) = (x, y, z) : getTrips yzv


after :: [Date] -> Date -> [Date]
ds `after` d = filter (d<) ds

filterExpired :: [Date] -> IO [Date]
filterExpired ds = liftM (ds `after`) getDay

getDates :: Leg a -> [Date]
getDates FiLeg{..} = dats
getDates FlLeg{..} = dats

legExpired :: Leg a -> Leg a -> IO Bool
legExpired l1 l2 = do nonexp1 <- filterExpired $ getDates l1;
                      nonexp2 <- filterExpired $ getDates l2;
                      return $ null nonexp1 && null nonexp2
                
                
instance Show (Leg a) where
    show FiLeg{..} = "Rate = 10"
    show FlLeg{..} = "Principal = 10000000" 
