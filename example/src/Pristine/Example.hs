{-# LANGUAGE MagicHash, UnboxedTuples, OverloadedStrings, DataKinds #-}

module Pristine.Example (example) where

-- --< Imports >-- {{{

-- GHC/base
import GHC.Exts (MutableByteArray#, fetchAddIntArray#, Int(..))

-- base
import Prelude hiding (words)
import Data.Bits (finiteBitSize)
import Data.Semigroup (stimes)
import Data.Function ((&))
import Data.Functor (void, (<&>))
import Data.Foldable (for_)
import Data.Traversable (for)
import Control.Monad.ST (RealWorld)

-- primitive
import Data.Primitive.ByteArray
  (MutableByteArray(..), newByteArray, setByteArray)
import Control.Monad.Primitive (primitive)

-- text
import qualified Data.Text         as T
import qualified Data.Text.Lazy.IO as Lazy

-- filepath
import System.FilePath ((</>), (<.>))

-- stm
import Control.Concurrent.STM (atomically)

-- ki
import Ki (Scope, scoped, fork, awaitAll)

-- pristine
import Pristine.Data
  ( mapData, ppEstimatedData, readFunDataFromField
  , preanalyse, estimateFunData, Estimated(..)
  , (:::)(..), Field(..), fromField, fromDoc
  )
import Pristine.Measurement
  ( Benchmark, runWith
  , list, (=:=), section
  , argument, uniform
  , Config(..), defConfig
  , Benchmarkable
  , Resource, res, env'
  , io_
  )
-- pristine-analysis
import Pristine.Analysis (tfEstimated, estimates, defDegree)
-- pristine-vis-Chart
import Pristine.Vis.Chart (layoutEstimated)
-- pristine-vis-Chart:rendering-cairo
import Pristine.Vis.Chart.Cairo (render, FileFormat(SVG))

-- }}}

-- --< Benchmark: mbr >-- {{{

-- The complete "memory barrier resolution" benchmark.
mbr :: Benchmark
mbr
  = section "MBR"
  . argument (uniform (1, 2000))
  $ "Uncontended" =:= uncontended
 <> "Contiguous"  =:= contiguous
 <> "Contended"   =:= contended

uncontended, contiguous, contended :: "n" ::: Int -> Benchmarkable

-- Run `fetchAdds` asyncronously on `n` different `mba`s.
uncontended (Row n) = ki \scope -> do
  -- We have access to `n` not just in the underlying `Action` but actually in
  -- the whole `Benchmarkable`.
  -- Leveraging this fact, we request a dynamic number of resources, all of
  -- which are fully initialised before measurement.
  n `stimes` do
    MutableByteArray mba <- zero
    pure $ io_ do
      void . fork scope $ fetchAdds mba 0

-- Run `fetchAdds` asyncronously on `n` different indices of the same `mba`.
contiguous (Row n) = ki \scope -> do
  MutableByteArray mba <- (n `zeroes`)
  pure $ io_ do
    for_ [0 .. n - 1] \i ->
      fork scope (fetchAdds mba i)

-- Run `fetchAdds` `n` times asyncronously on the same index of the same `mba`.
contended (Row n) = ki \scope -> do
  MutableByteArray mba <- zero
  (pure . io_) do
    n `times` void (fork scope (fetchAdds mba 0))

-- Convenient access to concurrency in a `Benchmarkable`.
ki :: (Scope -> Benchmarkable) -> Benchmarkable
ki k = res scoped >>= k <> pure . io_ . atomically . awaitAll

-- A single word MBA resource.
zero :: Resource (MutableByteArray RealWorld)
zero = zeroes 1

-- An n-word MBA resource.
zeroes :: Int -> Resource (MutableByteArray RealWorld)
zeroes n = env' do
  mba <- newByteArray (n `words`)
  setByteArray mba 0 n (0 :: Int)
  pure mba

-- The actual work: perform an atomic fetch-add increment 10,000 times.
fetchAdds :: MutableByteArray# RealWorld -> Int -> IO ()
fetchAdds mba (I# off) = 10_000 `times` primitive \s ->
  case fetchAddIntArray# mba off 1# s of
    (# s', _ #) -> (# s', () #)

-- Words in bytes.
words :: Int -> Int
words k = k * finiteBitSize (0 :: Word) `div` 8

-- A handy work multiplier.
times :: Applicative f => Int -> f () -> f ()
k `times` x | k <= 0    = pure ()
            | otherwise = x *> (k - 1) `times` x
infixr 6 `times`

-- }}}

-- --< Main >-- {{{

-- Our main.
example :: IO ()
example = do
  -- Run our `Benchmark`.
  runWith config mbr
  analyse

config :: Config
config = defConfig
  { samples   = 500
  , directory = "example-output" </> "csv"
  }

-- Analyse the benchmark data.
analyse :: IO ()
analyse = do

      -- Introspect our `Benchmark` for its components.
  let targets = list config mbr
  initResults <- for targets \(cnf, parents, name) ->
    -- Read the benchmark data from disk.
    readFunDataFromField cnf n parents name <&> \benchData ->
      ( name
      , benchData
        -- Prepare raw data for analysis.
      & preanalyse fromIntegral
        -- Estimate the three quartiles
      & estimateFunData (estimates defDegree [1/4])
      )

  let filepath measurement note
        = "example-output" </> "svg"
       </> "MBR " <> T.unpack measurement <> note <.> "svg"

  let
    -- Plot the benchmark data and their estimators against one another.
    title   = "Memory Barrier Resolution"
    layouts = layoutEstimated title (fromField n) initResults
  -- Render plots to SVG.
  for_ layouts \(measurement, l) -> do
    render SVG (filepath measurement "") 1000 1000 l

  let
    cleanResults = initResults <&> fmap \estimatedData ->
        estimatedData
        -- Use the estimates to detect and remove outliers.
      & mapData (tfEstimated (3/2))
        -- Discard the old analysis.
      & mapData sample
        -- Re-estimate all three quartiles and two extreme deciles from the
        -- clean data.
      & estimateFunData (estimates defDegree [1/4, 2/5])

  -- Generate and print a report with the results of the final analysis.
  let report = flip foldMap cleanResults \(name, estData) ->
        ppEstimatedData (Just 3) name estData (fromField n)
  Lazy.putStr (fromDoc report)

  let
    -- Plot the cleaned up benchmark data and final estimators.
    cleanLayouts = layoutEstimated
      (title <> " sans outliers") (fromField n) cleanResults
  -- Render clean plots to SVG.
  for_ cleanLayouts \(measurement, l) -> do
    render SVG (filepath measurement " sans outliers") 1000 1000 l

 where
  n :: Field "n" Int
  n = Field

-- }}}

