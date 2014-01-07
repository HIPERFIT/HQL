module Visualize where
import Graphics.EasyPlot
import Utils.Calendar
import Utils.Currency

type Coordinate = (Double, Double)
type Coordinates = [Coordinate]
type PlotData = (Int, Int)

transformCash (Cash v _) = v

transformDates [] = []
transformDates dates = transformDates' (head dates) (init dates)
  where transformDates' offsetDate (d:ds) = 0 : map (getYearOffset offsetDate) ds 

-- Move Payment declaration to Currency
toPlotData :: [(Date, Cash)] -> Coordinates
toPlotData dcs = zip dsCoords csCoords
  where csCoords = map transformCash cs
        dsCoords = transformDates ds
        (ds, cs) = unzip dcs

-- Test
p0 = read "2001-01-01" :: Date
p1 = read "2002-01-01" :: Date
p2 = read "2003-01-01" :: Date
p3 = read "2004-01-01" :: Date
p4 = read "2005-01-01" :: Date

c0 = Cash 10 USD
c1 = Cash 20 USD
c2 = Cash 30 USD
c3 = Cash 40 USD
c4 = Cash 50 USD

points = [p0,p1,p2,p3,p4]
cashes = [c0,c1,c2,c3,c4]
pms = zip points cashes
coords = toPlotData pms

testPlot = plot X11 $ Data2D [Title "Cashflow Diagram"] [] coords
