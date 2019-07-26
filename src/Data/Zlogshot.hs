{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Zlogshot
  ( module Data.Zlogshot
  , module Data.Zlogshot.Types
  , module Data.Zlogshot.IO
  )
where

import           Control.Monad
import           Data.Dependent.Sum
import           Data.Zlogshot.IO
import           Data.Zlogshot.Types
import           Graphics.Rendering.Chart.Backend.Diagrams
import           Graphics.Rendering.Chart.Easy     hiding ( argument
                                                          , beside
                                                          , label
                                                          )
import           Graphics.Rendering.Chart.Grid

lifetimeOf :: Integer -> Integer -> Integer
lifetimeOf k n = if x == n then k * n else lifetimeOf k (n - x)
  where x = 2 ^ (floor (logBase 2 (fromIntegral n) :: Double) :: Integer)

survivingGens :: Integer -> Integer -> [Integer]
survivingGens k cur = filter (\g -> lifetimeOf k g + g > cur) [1 .. cur]

writeSvg' :: Int -> Integer -> Integer -> IO Int
writeSvg' bins k cur = writeSvg (survivingGens k cur) bins

writeSvg :: [Integer] -> Int -> IO Int
writeSvg (fmap (fromIntegral @_ @Double) -> gens) bins = do
  void
    $ renderableToFile (def -- { _fo_size = (1600, 1200) }
                           ) "out.svg"
    $ fillBackground def
    $ gridToRenderable
    $ layoutToGrid
    $ execEC
    $ plot
    $ pure
    $ histToPlot
    $ defaultPlotHist { _plot_hist_values     = gens
                      , _plot_hist_drop_lines = True
                      , _plot_hist_bins       = bins
                      }
  return $ length gens

makeExpiringSnapshot
  :: Snapshot -> Integer -> Generation -> SnapshotDepth -> IO ()
makeExpiringSnapshot snap coefficient gen = makeSnapshot
  snap
  [ BackupGeneration ==> gen
  , BackupExpiration ==> gen + Generation
    (lifetimeOf coefficient $ unGeneration gen)
  ]
