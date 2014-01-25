
-- |
-- Module:      Utils.Graphics.Visualize
-- Copyright:   (c) 2013 HIPERFIT
-- License:     BSD-3
-- Maintainer:  Johan Astborg, Andreas Bock <bock@andreasbock.dk>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working with interest rates
module Utils.Graphics.Visualize where
{--

import Graphics.EasyPlot
import Utils.Calendar
import Utils.Currency

type Coordinate = (Double, Double)
type Coordinates = [Coordinate]
type PlotData = (Int, Int)

transformCash (Cash v _) = v

-- | Transform dates in a list
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

--}

import qualified Graphics.Gnuplot.Advanced as GP

import qualified Graphics.Gnuplot.MultiPlot as MultiPlot

import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Frame.OptionSet.Style as OptsStyle
import qualified Graphics.Gnuplot.Frame.OptionSet.Histogram as Histogram

import qualified Graphics.Gnuplot.Graph as Graph

import qualified Graphics.Gnuplot.Plot.ThreeDimensional as Plot3D
import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D

import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import Graphics.Gnuplot.Plot.TwoDimensional (linearScale, )

import qualified Graphics.Gnuplot.LineSpecification as LineSpec

import qualified Data.Time as Time

import Control.Monad (liftM2, )
import Data.Array (listArray, )
import Data.Foldable (foldMap, )
import Data.Monoid (mappend, mconcat, )


cashflowDiagram :: [Double] -> Frame.T (Graph2D.T Int Double)
cashflowDiagram values = 
   Frame.cons (
      Opts.title "HQL Cashflow Diagram" $      
      Histogram.clusteredGap 3 $
      --Opts.boxwidthAbsolute 0.9 $
      Opts.yRange2d (0,(maximum values)+(minimum values)) $
      OptsStyle.fillBorderLineType (-1) $
      
      OptsStyle.fillSolid $
      Opts.keyOutside $
      Opts.xTicks2d
         [("0", 0), ("1", 1), ("2", 2), ("3", 3), ("4", 4), ("5", 5)] $
      Opts.deflt) $
      foldMap (\(title,dat) ->
      fmap (Graph2D.lineSpec (LineSpec.title title LineSpec.deflt)) $
      Plot2D.list Graph2D.histograms dat) $
   ("Cashflow", values) :
   []

--test1 :: IO ()
--test1 = sequence_ $ GP.plotDefault $ cashflowDiagram [100,100,100,100,1100] : []

testPlot = GP.plotDefault $ cashflowDiagram [500,500,500,500,1000]
