{-# LANGUAGE OverloadedStrings #-}

module Pristine.Sanity (sanity) where

-- --< Imports >-- {{{

-- base
import Prelude hiding (words)
import Data.List (sort)
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.Traversable (for)

-- text
import           Data.Text   (Text)
import qualified Data.Text as T

-- vector
import           Data.Vector (Vector, (!))
import qualified Data.Vector as V

-- filepath
import System.FilePath ((</>))

-- pristine
import Pristine.Measurement
  ( Benchmark, runWith
  , section, (=:=), list
  , Config(..), defConfig
  )
import Pristine.Compat (nf)
import Pristine.Data
  ( Data(..), readData
  , TimingData(..), Time(..)
  )

-- }}}

-- --< Benchmarks >-- {{{

control :: Benchmark
control = "Control" =:= mempty

sums :: Benchmark
sums
  = section "Sums"
  $ flip foldMap [0 :: Int .. 7] \n ->
    let name = "Sum 1 through 10e" <> T.pack (show n)
    in  name =:= nf sum [1 :: Int .. 10^n]

-- }}}

-- --< Main >-- {{{

sanity :: IO ()
sanity = do
  runWith config (control <> sums)
  report

config :: Config
config = defConfig{ directory = "sanity-output" </> "csv" }

-- assuming sorted input
quartiles :: Ord a => Vector a -> (a, a, a)
quartiles xs = (q1, q2, q3)
 where
  n = V.length xs
  -- simplified
  q1 = xs ! (1 * n `div` 4)
  q2 = xs ! (2 * n `div` 4)
  q3 = xs ! (3 * n `div` 4)

fence :: (Ord a, Integral a) => a -> a -> Vector a -> Vector a
fence q1 q3 = V.filter \x -> x >= fenceLo && x <= fenceHi
 where
  iqr = q3 - q1
  r = 3 * iqr `div` 2
  fenceLo = q1 - r
  fenceHi = q3 + r

analyse :: Benchmark -> IO [(Text, Time, Time, Time)]
analyse bm = for (list config bm) \(cnf, parents, name) -> do
  MkData{timingData} <- readData cnf parents name
  case timingData of
    Nothing                -> error "No timing data."
    Just MkTimingData{cpu} -> pure (name, q1, q2, q3)
     where
      sorted = (V.fromList . sort . V.toList) cpu
      (iq1, _, iq3) = quartiles sorted
      fenced = fence iq1 iq3 sorted
      (q1, q2, q3) = quartiles fenced

reportQuantiles :: (Text, Time, Time, Time) -> IO ()
reportQuantiles (name, q1, q2, q3)
  = putStr . unlines
  $ [ T.unpack name
    , "  Q_1 = " ++ nano q1
    , "  Q_2 = " ++ nano q2
    , "  Q_3 = " ++ nano q3
    , ""
    ]
 where
  nano t = show (nanoseconds t) ++ " ns"

report :: IO ()
report = do
  [ctrl@(_, c1, c2, c3)] <- analyse control
  reportQuantiles ctrl

  uncorrected <- analyse sums
  let
    corrected = uncorrected <&> \(n, q1, q2, q3) ->
      (n, q1 - c3, q2 - c2, q3 - c1)
  traverse_ reportQuantiles corrected

-- }}}

