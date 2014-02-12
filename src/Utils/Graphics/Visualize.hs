-- Module:      Utils.Graphics.Visualize
-- Copyright:   (c) Johan Astborg, Andreas Bock
-- License:     BSD-3
-- Maintainer:  Johan Astborg <joastbg@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
module Utils.Graphics.Visualize where

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

-- HQL
import Utils.Currency

getAmount (Cash amount _) = amount

diagram :: [(String, Cash)] -> Frame.T (Graph2D.T Int Double)
diagram values =
   Frame.cons (
      Opts.title "HQL Cashflow Diagram" $
      Histogram.clusteredGap 3 $
      Opts.yRange2d (0,(maximum $ map (getAmount . snd) values)+(minimum $ map (getAmount . snd) values)) $
      OptsStyle.fillBorderLineType (-1) $
      OptsStyle.fillSolid $
      Opts.keyInside $
      Opts.xTicks2d
         (zipWith (\a b -> (a,b)) (map fst values) [0..]) $
      Opts.deflt) $
      foldMap (\(title,dat) ->
      fmap (Graph2D.lineSpec (LineSpec.title title LineSpec.deflt)) $
      Plot2D.list Graph2D.histograms dat) $
      [("Cashflow", (map (getAmount . snd) values))] 

-- Example of a cashflow diagram:
-- cash1 = Cash 500 USD
-- cash2 = Cash 1500 USD
-- testData = [("2014-06-01", cash1),("2015-01-01", cash1),("2015-06-01", cash1),("2016-01-01", cash1),("2016-06-01", cash2)]
-- testPlot = GP.plotDefault $ diagram testData
graphPayments f = GP.plotDefault $ diagram f'
  where f' = map (\(d,c) -> (show d, c)) f
