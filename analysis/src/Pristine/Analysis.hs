
-- --< Header >-- {{{

{-# LANGUAGE RankNTypes, PatternSynonyms #-}

{- |

Description : Simple, robust, nonparametric statistical analysis
Copyright   : (c) 2026 L. S. Leary

Simple statistical analysis for single-argument benchmarks which is robust to outliers and makes no assumptions about underlying distributions.

-}

-- }}}

-- --< Exports >-- {{{

module Pristine.Analysis (

  -- * Outlier Removal
  tfEstimated,
  tukeysFences,

  -- * Estimation
  estimates,
  estimate,
  -- ** Degree
  Degree(Exactly,CrossValidate),
  defDegree,

  -- * Cross Validation
  cvSelect,
  cvLoss,

  -- * Quantile Regression
  qrFit,

) where

-- }}}

-- --< Imports >-- {{{

-- base
import Prelude hiding (head)
import Data.List (sort, sortOn)
import Data.List.NonEmpty (head, group)
import Data.Functor ((<&>))

-- vector
import           Data.Vector.Storable (Vector, (!))
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed  as U

-- hmatrix
import Numeric.LinearAlgebra (linearSolveLS)
import Numeric.LinearAlgebra.Data (fromColumns, asColumn, flatten)

-- poly
import Data.Poly (UPoly, toPoly, unPoly, eval)

-- pristine
import Pristine.Data
  ( Estimated(..), Estimator(..), QuantileRange(..)
  , Fun(..), coerceFun, Stored(..)
  )

-- }}}

-- --< Outlier Removal >-- {{{

-- | Remove outliers according to `tukeysFences`, using the `QuantileRange` closest to the traditional inter-quartile range.
--   Errors if there are no `QuantileRange`s in the `Estimated` data.
tfEstimated
  -- | \( k \geq 0 \). See `tukeysFences`.
  :: Double
  -> Estimated Stored x y
  -> Estimated Stored x y
tfEstimated k MkEstimated{sample,estimated}
  = "tfEstimated" `refuse` ["k" @ k ~ "< 0" @ (< 0)]
  $ case qrs of
    [  ] -> die "tfEstimated" "no quantile ranges"
    qr:_ -> MkEstimated
      { sample    = coerceFun . tukeysFences k qr . coerceFun $ sample
      , estimated = estimated
      }
 where
  MkEstimator{estQuantiles} = estimated
  iqrness MkQuantileRange{probabilityRadius} = abs (0.25 - probabilityRadius)
  qrs = sortOn iqrness estQuantiles


-- | Remove outliers falling outside /Tukey's fences/:
-- \[
--   [ Q_{lo} - k (Q_{hi} - Q_{lo}), Q_{hi} + k (Q_{hi} - Q_{lo}) ]
-- \]
tukeysFences
  -- | \( k \geq 0 \).
  --   The standard recommendation is to use \( k = \frac{3}{2} \).
  :: Double
  -- | \( Q_{lo} \) and \( Q_{hi} \).
  --   In standard practice these are the first and third quartiles, i.e. `probabilityRadius` \( r_p = \frac{1}{4} \).
  -> QuantileRange
  -> Fun Vector Double Double
  -> Fun Vector Double Double
tukeysFences k MkQuantileRange{lowerQuantile,upperQuantile} MkFun{xvals,yvals}
  = MkFun
    { xvals = xvals `sans` outliers
    , yvals = yvals `sans` outliers
    }
 where
  !outliers = S.zipWith outlier xvals yvals
  outlier x y = y < q_lo - r || y > q_hi + r
   where
    !q_lo = eval lowerQuantile x
    !q_hi = eval upperQuantile x
    !r = k * (q_hi - q_lo)

sans :: S.Storable a => Vector a -> Vector Bool -> Vector a
zs `sans` unwanted = S.ifilter (\i _ -> not (unwanted ! i)) zs

-- }}}

-- --< Estimation >-- {{{

-- | Given the `Degree` and a set of probability radii, produce @estimates@ for the median and quantiles of some functional data.
estimates
  :: Degree
  -- | Probability radii in range \( (0, \frac{1}{2}) \).
  -> [Double]
  -> Fun Vector Double Double
  -> Estimator
estimates deg rp0 xys
  = "estimates" `refuse`
    ["probability radii" @ rp0 ~ "not all in range (0, 0.5)" @ (/= rp1)]
  $ MkEstimator
    { estMedian    = quantile 0.5
    , estQuantiles = rp2 <&> \delta -> MkQuantileRange
      { probabilityRadius = delta
      , lowerQuantile     = quantile (0.5 - delta)
      , upperQuantile     = quantile (0.5 + delta)
      }
    }
 where
  quantile τ = estimate deg τ xys
  rp2 = head <$> group (sort rp1)
  rp1 = filter (\x -> 0 < x && x < 0.5) rp0

-- | Given the `Degree` and \( \tau \), @estimate@ the quantile \( Q_\tau \) for some functional data with a polynomial.
estimate
  :: Degree
  -- | \( \tau \in (0, 1) \).
  -> Double
  -> Fun Vector Double Double
  -> UPoly Double
estimate deg τ xys = case deg of
  Exactly degree        -> fit degree xys
  CrossValidate folds λ -> cvSelect folds λ xys fit loss
 where
  fit = qrFit τ
  loss p x y = check τ (y - eval p x)

-- The check loss ρ_τ.
-- 0 < τ < 1
check :: Double -> Double -> Double
check τ r
  | r < 0     = (τ - 1) * r
  | otherwise =  τ      * r

-- }}}

-- --< Estimation / Degree >-- {{{

-- | Specification of polynomial degree.
data Degree
  -- | @Exactly@ \( n \).
  = Exactly !Int
  | CrossValidate_ !Int !Double
 deriving (Show, Read, Eq, Ord)

-- | @defDegree = `CrossValidate` 10 (1/5)@
defDegree :: Degree
defDegree = CrossValidate 10 (1/5)

{- |
Select degree according to \( k \)-fold cross validation with penalty \( \lambda \).

Unpenalised cross validation does not necessarily suffice to select the most appropriate model, as it will accept arbitrary increases in complexity for insignificant reductions in loss.
As such, we regularise cross validation by penalising polynomial degree \( n \) by a factor of \( (1 + \lambda)^n \).
-}
{-# COMPLETE Exactly, CrossValidate #-}
pattern CrossValidate
  -- | \( k > 1 \).
  :: Int
  -- | \( \lambda \geq 0 \).
  -> Double
  -> Degree
pattern CrossValidate k λ <- CrossValidate_ k λ
  where CrossValidate = crossValidate

crossValidate :: Int -> Double -> Degree
crossValidate k λ
  = "CrossValidate" `refuse`
    [ "k" @ k ~ "<= 1" @ (<= 1)
    , "λ" @ λ ~  "< 0" @ ( < 0)
    ]
  $ CrossValidate_ k λ

-- }}}

-- --< Cross Validation >-- {{{

-- | Select polynomial degree via cross validation.
cvSelect
  -- | \( k > 1 \).
  --   See `CrossValidate`.
  :: Int
  -- | \( \lambda \geq 0 \).
  --   See `CrossValidate`.
  -> Double
  -> Fun Vector Double Double
  -- | Polynomial fitting function for a given degree.
  -> (Int -> Fun Vector Double Double -> UPoly Double)
  -- | Loss of \( p \) at \( x_i, y_i \).
  -> (UPoly Double -> Double -> Double -> Double)
  -- | The fitted polynomial of the selected degree.
  -> UPoly Double
cvSelect k λ xys fit loss
  = "cvSelect" `refuse`
    [ "k" @ k ~ "<= 1" @ (<= 1)
    , "λ" @ λ ~  "< 0" @ ( < 0)
    ]
  $ go 0 (lossAt 0)
 where
  lossAt n = (cvLoss k xys (fit n) loss + 1) * (1 + λ)^n
  go n lossAtn
    | lossAtnp1 < lossAtn = go (n + 1) lossAtnp1
    | otherwise           = fit n xys
   where
    !lossAtnp1 = lossAt (n + 1)

-- | Calculate the \( k \)-fold cross validation loss for a given polynomial fitting model.
cvLoss
  -- | \( k > 1 \).
  --   See `CrossValidate`.
  :: Int
  -> Fun Vector Double Double
  -- | Polynomial fitting model.
  -> (Fun Vector Double Double -> UPoly Double)
  -- | Loss of \( p \) at \( x_i, y_i \).
  -> (UPoly Double -> Double -> Double -> Double)
  -> Double
cvLoss k xys fit loss = sum $ perFold \train test ->
  S.sum (S.zipWith (loss (fit train)) (xvals test) (yvals test))
 where
  n = S.length (xvals xys)
  perFold f = zipWith mkFuns (foldSplits (xvals xys)) (foldSplits (yvals xys))
   where
    mkFuns (train_xs, test_xs) (train_ys, test_ys)
      = f MkFun{ xvals = train_xs, yvals = train_ys }
          MkFun{ xvals =  test_xs, yvals =  test_ys }
  foldSplits zs = testSlices <&> \(i, s) -> case S.splitAt i zs of
    (train1, testTrain2) -> case S.splitAt s testTrain2 of
      (test, train2) -> (train1 <> train2, test)
   where
    testSlices = zip (scanl (+) 0 sizes) sizes
     where
      sizes = zipWith (+) (replicate k q) (replicate r 1 ++ repeat 0)
       where (q, r) = n `divMod` k

-- }}}

-- --< Quantile Regression >-- {{{

-- | Fit a degree \( n \) polynomial to the quantile \( Q_\tau \).
qrFit
  -- | \( \tau \in (0, 1) \).
  :: Double
  -- | \( n \geq 0 \).
  -> Int
  -> Fun Vector Double Double
  -> UPoly Double
qrFit τ n xys
  = "qrFit" `refuse`
    [ "τ" @ τ ~ "<= 0" @ (<= 0)
    , "τ" @ τ ~ ">= 1" @ (>= 1)
    , "n" @ n ~  "< 0" @ ( < 0)
    ]
  $ go (olsFit n xys)
 where
  -- geometric mean magnitude
  !α = exp
     $ S.sum (S.map (log . pos . abs) (xvals xys))
     / fromIntegral (S.length (xvals xys))
  epsilon = 1e-8
  go p
    | polyMet 2 α p p' < epsilon =    p'
    | otherwise                  = go p'
   where
    !p' = wlsFit n ws xys
     where
      ws = S.zipWith weight (xvals xys) (yvals xys)
       where
        weight x y = check τ (inv (y - eval p x))

-- A scale-invariant (not quite) p-metric for polynomials specialised to the
-- vicinity of α.
-- p >= 1
polyMet :: Double -> Double -> UPoly Double -> UPoly Double -> Double
polyMet p α q r
  = sqr (polyNorm p α (r - q))
  / pos (polyNorm p α q * polyNorm p α r)

-- A p-norm for polynomials specialised to the vicinity of α.
-- p >= 1
polyNorm :: Double -> Double -> UPoly Double -> Double
polyNorm p α q
  = "polyNorm" `refuse` ["p" @ p ~ "< 1" @ (< 1)]
  $ root p (eval (polyL p q) (abs α))
 where
  root  1 x =      x
  root  2 x = sqrt x
  root  m x =      x ** (1 / m)
  polyL 1 = mapPoly       abs
  polyL 2 = mapPoly       sqr
  polyL m = mapPoly \c -> abs c ** m

-- }}}

-- --< Internal: Least Squares >-- {{{

-- n >= 0
olsFit :: Int -> Fun Vector Double Double -> UPoly Double
olsFit n xys = wlsFit n (S.replicate num 1) xys
 where num = S.length (xvals xys)

-- n >= 0
wlsFit
  :: Int -> Vector Double -> Fun Vector Double Double -> UPoly Double
wlsFit n ws MkFun{xvals,yvals}
  = toPoly . S.convert . flatten
  . linearSolveLS xpows . asColumn
  . S.zipWith (*) yvals
  $ ws'
 where
  !ws' = S.map sqrt ws
  xpows = fromColumns . take (n + 1) $ iterate (S.zipWith (*) xvals) ws'

-- }}}

{-
-- --< Quantile Regression / Simplex >-- {{{

-- | Fit a degree \( n \) polynomial to the quantile \( Q_\tau \).
qrFitSimplex
  -- | \( \tau \in (0, 1) \).
  :: Double
  -- | \( n \geq 0 \).
  -> Int
  -> Fun Vector Double Double
  -> UPoly Double
qrFitSimplex τ n MkFun{xvals,yvals}
  = "qrFitSimplex" `refuse`
    [ "τ" @ τ ~ "<= 0" @ (<= 0)
    , "τ" @ τ ~ ">= 1" @ (>= 1)
    , "n" @ n ~  "< 0" @ ( < 0)
    ]
  $ case solution of
      Optimal (_, lmuv) -> extract lmuv
      s                 -> die "qrFitSimplex" (show s)
 where
  extract lmuv = case splitAt (n + 1) lmuv of
    (l, muv) -> toPoly $ U.fromListN (n + 1) (zipWith (-) l muv)

  solution  = exact objective constraints []
  objective = Minimize d
   where
    d = replicate (2 * (n + 1)) 0
     ++ replicate       num     τ
     ++ replicate       num    (1 - τ)

  constraints = Sparse (zipWith con [1..] xrows)
   where
    con i xi = lhs :==: y i
     where
      lhs = zip (S.toList xi ++ S.toList (-xi)) [1..] ++ [1 # u i, -1 # v i]
    u i = 2 * (n + 1) + i
    v i = u i + num
    y i = yvals ! (i - 1)
    xrows = toRows . fromColumns . take (n + 1)
          $ iterate (S.zipWith (*) xvals) (S.replicate num 1)

  num = S.length xvals

-- }}}
-}

-- --< Internal: Util >-- {{{

mapPoly
  :: (U.Unbox a, U.Unbox b, Num b, Eq b)
  => (a -> b) -> UPoly a -> UPoly b
mapPoly f = toPoly . U.map f . unPoly

pos :: Double -> Double
pos = max 1e-9

inv :: Double -> Double
inv !r = signum r / pos (abs r)

sqr :: Num a => a -> a
sqr !x = x * x

refuse :: String -> [String -> b -> b] -> b -> b
(name `refuse` errors) k = foldr ($ name) k errors

(~) :: Show a => (String, a) -> (String, a -> Bool) -> String -> b -> b
(var, x) ~ (cond, p)
  | p x       = \name -> die name (var ++ " = " ++ show x ++ " " ++ cond)
  | otherwise = \_    -> id
infix 1 ~

(@) :: a -> b -> (a, b)
(@) = (,)

die :: String -> String -> a
die name msg = error $ "Pristine.Analysis." ++ name ++ ": " ++ msg ++ "."

-- }}}

