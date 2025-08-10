
-- --< Header >-- {{{

{-# LANGUAGE RankNTypes, OverloadedStrings #-}

{- |

Description : Plotting for both pristine and processed data
Copyright   : (c) 2026 L. S. Leary

Plotting for both pristine and processed data.

-}

-- }}}

-- --< Exports >-- {{{

module Pristine.Vis.Chart (

  -- * Plotting

  -- ** Data
  layoutFun,
  layoutEstimated,

  -- ** Scatter
  Scatter(..),
  layout,
  plot,

  -- *** Bounds
  Bounds(..),
  pointBounds,
  findBounds,

) where

-- }}}

-- --< Imports >-- {{{

-- base
import Data.Coerce (Coercible, coerce)
import Data.Int (Int64)
import Data.Ratio (denominator)
import Data.Ord (Down(..))
import Data.Tuple (swap)
import Data.Maybe (fromMaybe, mapMaybe, catMaybes)
import Data.Foldable (toList, foldl')
import Data.Functor ((<&>))
import Data.Bifunctor (Bifunctor(..))
import Control.Arrow ((&&&))

-- containers
import           Data.IntMap.Strict (IntMap, (!?))
import qualified Data.IntMap.Strict as I

-- text
import           Data.Text        (Text)
import qualified Data.Text      as T
import qualified Data.Text.Lazy as Lazy

-- vector
import           Data.Vector (Vector)
import qualified Data.Vector          as G
import qualified Data.Vector.Storable as S

-- poly
import Data.Poly (eval)

-- data-default-class
import Data.Default.Class (Default(def))

-- colour
import Data.Colour (Colour, AffineSpace(..), opaque, withOpacity)
import Data.Colour.Names (red, green, blue, black)

-- Chart
import Graphics.Rendering.Chart
  (PointStyle(..), PointShape(..), LineStyle(..), FillStyle(..), FontStyle(..))
import Graphics.Rendering.Chart.Layout (Layout(..), LayoutAxis(..))
import Graphics.Rendering.Chart.Axis.Types
  (PlotValue(..), AxisStyle(..), AxisData, makeAxis)
import Graphics.Rendering.Chart.Plot.Types (ToPlot(..), Plot(..), joinPlot)
import Graphics.Rendering.Chart.Plot.Points (PlotPoints(..))
import Graphics.Rendering.Chart.Plot.Lines (PlotLines(..))
import Graphics.Rendering.Chart.Plot.FillBetween (PlotFillBetween(..))
import Graphics.Rendering.Chart.Legend (LegendStyle(..), LegendOrientation(..))

-- pristine
import Pristine.Data
  ( Data(..), TimingData(..), Time(..), StatData(..), Space(..)
  , Fun(..), mapFun, Stored(..)
  , ppPoly, Estimated(..), Estimator(..), scaleEst
  , QuantileRange(..), ppQR
  , fromDoc, text, FPFormat(..), formatRealFloat,
  )

-- }}}

-- --< Plotting / Data >-- {{{

-- | Lay out `Time` and `Pristine.Data.Stats` measurements as scatter plots.
layoutFun
  :: (PlotValue x, Show x)
  -- | Title.
  => Text
  -- | The name of the variable @x@.
  -> Text
  -- | Named measurements of functions in @x@.
  -> [(Text, Data (Fun Vector x))]
  -- | Named and laid-out plots.
  -> [(Text, Layout Double Double)]
layoutFun = mkLayout @(Coercible Int64) mkScatters
 where
  mkScatters
    :: Coercible Int64 y
    => [(Text, t)] -> (t -> Fun Vector x y)
    -> [Scatter x Int64]
  mkScatters data_ f = coerce $ data_ <&> \(n, s) ->
    mkScatter @Null G.toList n (f s)

-- | Lay out `Time` and `Pristine.Data.Stats` measurements with their estimators.
layoutEstimated
  :: (PlotValue x, Show x)
  -- | Title.
  => Text
  -- | The name of the variable @x@.
  -> Text
  -- | Named measurements of functions in @x@ with estimators.
  -> [(Text, Data (Estimated Stored x))]
  -- | Named and laid-out plots.
  -> [(Text, Layout Double Double)]
layoutEstimated = mkLayout @Null mkScatters
 where
  mkScatters
    :: [(Text, b)] -> (b -> Estimated Stored w y) -> [Scatter Double Double]
  mkScatters data_ f = data_ <&> \(n, t) ->
    let MkEstimated{sample,estimated} = f t
    in  (mkScatter @S.Storable S.toList n $ unstore sample)
          { estimator = Just estimated }
   where unstore = mapFun unMkStored unMkStored

mkLayout
  :: forall c x y f g
   . (PlotValue x, Show x, PlotValue y, c Time, c Space)
  -- | Scatter maker.
  => (forall d t. c t => [(Text, d f)] -> (d g -> g t) -> [Scatter x y])
  -- | Title.
  -> Text
  -- | The name of the variable @x@.
  -> Text
  -- | Named measurements of functions in @x@ with estimators.
  -> [(Text, Data f)]
  -- | Named and laid-out plots.
  -> [(Text, Layout Double Double)]
mkLayout mkScatters title xName benchmarks = catMaybes
  [ ltx "wall time"         -3 "s" wallScatters
  , ltx "CPU time"          -3 "s" cpuScatters
  , ltx "allocations"        0 "B" allocScatters
  , ltx "copied"             0 "B" copiedScatters
  , ltx "mutator wall time" -3 "s" mutWallScatters
  , ltx "mutator CPU time"  -3 "s" mutCpuScatters
  ]
 where
  ltx = layout @x @y title xName

  extractData f = mapMaybe (\(n, data_) -> (n,) <$> f data_) benchmarks
  timingData_   = extractData timingData
  statData_     = extractData statData

  wallScatters    = mkScatters timingData_ wall
  cpuScatters     = mkScatters timingData_ cpu
  allocScatters   = mkScatters statData_   allocated
  copiedScatters  = mkScatters statData_   copied
  mutWallScatters = mkScatters statData_   mutWall
  mutCpuScatters  = mkScatters statData_   mutCPU

-- }}}

-- --< Plotting / Scatter >-- {{{

-- | Named sets of 2D points with optional `Estimator`s.
data Scatter x y = MkScatter
  { name      :: !Text
  , scatter   :: [(x, y)]
  , estimator :: Maybe Estimator
  }
 deriving Functor

instance Bifunctor Scatter where
  bimap f g sc = sc{ scatter = map (bimap f g) (scatter sc) }

class    Null a
instance Null a

mkScatter
  :: (c x, c y)
  => (forall z. c z => v z -> [z])
  -> Text -> Fun v x y -> Scatter x y
mkScatter list name MkFun{xvals,yvals} = MkScatter
  { name
  , scatter   = list xvals `zip` list yvals
  , estimator = Nothing
  }

-- | Lay out comparable sets of points into a scatter plot.
layout
  :: (PlotValue x, Show x, PlotValue y)
  -- | Title.
  => Text
  -- | The name of the variable @x@.
  -> Text
  -- | Label for the y-axis.
  -> Text
  -- | Unit magnitude prefix \( \log_{1000} \) of the quantity on the y-axis.
  --   E.g. \( -2 \sim \mu \); \( -1 \sim m \);
  --        \(  1 \sim  k  \); \(  2 \sim M \).
  -> Int
  -- | /Base/ unit of the quantity on the y-axis.
  -> Text
  -- | Named data sets.
  -> [Scatter x y]
  -> Maybe (Text, Layout Double Double)
layout _     _     _      _     _    [      ] = Nothing
layout title xName yLabel kmag yUnit scatters = Just
  ( yLabel
  , plot (title <> ": " <> yLabel) xLabel yLabel' bounds' scatters'
  )
 where
  xLabel  = xName
  yLabel' = yLabel <> " (" <> prefix <> yUnit <> ")"
   where
    prefix = foldMap snd (siPrefixes !? (kscale + kmag))
  kscale = floor (logBase @Double 1000 mid_y)
  mid_y = toValue (min_y bounds) + toValue (max_y bounds)
  rescale = (lam *) . toValue
  lam = 1000^^(-kscale)
  bounds' = rescale <$> bounds
  bounds
    = fromMaybe dummyBounds
    . foldMap (findBounds . scatter)
    $ scatters
  scatters' = scatters <&> \s@MkScatter{scatter,estimator} -> s
    { scatter   = second rescale <$> scatter
    , estimator = scaleEst lam <$> estimator
    }

-- | Plot `Scatter`s against one another.
plot
  :: forall x y
  .  ( PlotValue x, Show x
     , PlotValue y, Show y
     )
  -- | Plot title.
  => Text
  -- | Label of the x-axis.
  -> Text
  -- | Label of the y-axis.
  -> Text
  -- | Bounds for the supplied points.
  -> Bounds x y
  -- | The points to plot.
  -> [Scatter x y]
  -> Layout Double Double
plot title xLabel yLabel bounds scatters = theLayout
 where
  toDouble :: Bifunctor b => b x y -> b Double Double
  toDouble = bimap toValue toValue
  MkBounds{min_x,min_y,max_x,max_y} = toDouble bounds
  defLayout = def :: Layout Double Double
  defyAxis = _layout_y_axis defLayout
  defxAxis = _layout_x_axis defLayout
  theLayout = defLayout
    { _layout_title       = T.unpack title
    , _layout_title_style = (_layout_title_style defLayout){ _font_size = 20 }
    , _layout_plots       = pure . foldr joinPlot' nullPlot
                          . map (foldr joinPlot nullPlot)
                          . reverse . foldr zippend []
                          $ plots
    , _layout_y_axis      = defyAxis
      { _laxis_title       = T.unpack yLabel
      , _laxis_title_style = (_laxis_title_style defyAxis){ _font_size = 17 }
      , _laxis_style       = axisStyle
      , _laxis_generate    = \_ -> axis 10 min_y max_y
      }
    , _layout_x_axis      = defxAxis
      { _laxis_title       = T.unpack xLabel
      , _laxis_title_style = (_laxis_title_style defxAxis){ _font_size = 17 }
      , _laxis_style       = axisStyle
      , _laxis_generate    = \_ -> axis 15 min_x max_x
      }
    , _layout_legend      = Just legendStyle
    }
   where
    axisStyle = def
      { _axis_line_style  = lineStyle black
      , _axis_label_style = (_axis_label_style def){ _font_size = 12 }
      }
    legendStyle = def
      { _legend_plot_size   = 30
      , _legend_orientation = LORows 1
      , _legend_label_style = (_legend_label_style def){ _font_size = 15 }
      }

  plots = zip3 (map toDouble scatters) (length scatters `colours`) shapes
    <&> \(MkScatter{name,scatter,estimator}, colour, shape) -> do
    let
      scatPlot = toPlot def
        { _plot_points_title  = ' ':T.unpack name
        , _plot_points_style  = pointStyle
        , _plot_points_values = scatter
        }
       where
        pointStyle = def
          { _point_color  = opaque colour
          , _point_radius = 4
          , _point_shape  = shape
          }
    [scatPlot]:do
      MkEstimator{estMedian,estQuantiles} <- toList estimator
      let
        ppQuant = formatRealFloat Fixed    (Just 2)
        ppCoeff = formatRealFloat Exponent (Just 2)
        medName = "median " <> text yLabel <> " = "
                            <> ppPoly ppCoeff estMedian xLabel
        linePlot = toPlot def
          { _plot_lines_title  = ' ':Lazy.unpack (fromDoc medName)
          , _plot_lines_style  = lineStyle colour
          , _plot_lines_values = [curve (eval estMedian)]
          }
      [linePlot]:do
        qr@MkQuantileRange{lowerQuantile,upperQuantile} <- estQuantiles
        let
          qrName = ppQR ppQuant ppCoeff qr xLabel yLabel
          fillPlot = toPlot def
            { _plot_fillbetween_title  = ' ':Lazy.unpack (fromDoc qrName)
            , _plot_fillbetween_style  = def
              { _fill_color = colour `withOpacity` (1/3)
              }
            , _plot_fillbetween_values
              = curve (eval lowerQuantile &&& eval upperQuantile)
            }
        pure [fillPlot]

  lineStyle colour = def
    { _line_color = opaque colour
    , _line_width = 1.4
    }

  axis :: (PlotValue z, RealFrac z, Show z) => Int -> z -> z -> AxisData z
  axis targetNum lo hi = makeAxis omitEnds (lo:ts ++ [hi], ts, ts)
   where
    (lo', _, tickSize, ts) = ticks targetNum lo hi
    pp | integral lo'
       , integral tickSize = show @Int . round
       | otherwise         = show
     where integral x = denominator x == 1
    omitEnds [    ] = []
    omitEnds (_:xs) = "":omitLast xs
     where
      omitLast [    ] = [  ]
      omitLast [_   ] = [""]
      omitLast (y:ys) = pp y:omitLast ys

  curve f = [min_x, min_x + resolution .. max_x] <&> \x -> (x, f x)
   where resolution = (max_x - min_x) / 2000

zippend :: Monoid a => [a] -> [a] -> [a]
zippend (x:xs) (y:ys) = x <> y:zippend xs ys
zippend    []     ys  = ys
zippend    xs     []  = xs

joinPlot' :: Plot x y -> Plot x y -> Plot x y
joinPlot' (Plot rp lp xysp) (Plot rq lq xysq) = Plot
  { _plot_render     = \a -> rp a >> rq a
  , _plot_legend     = lq <> lp
  , _plot_all_points = xysp <> xysq
  }

nullPlot :: Plot x y
nullPlot = Plot (\_ -> pure ()) mempty mempty

-- }}}

-- --< Plotting / Scatter / Bounds >-- {{{

-- | 2D bounds.
data Bounds x y = MkBounds
  { min_x :: !x
  , min_y :: !y
  , max_x :: !x
  , max_y :: !y
  }
 deriving Functor

instance Bifunctor Bounds where
  bimap f g MkBounds{min_x,min_y,max_x,max_y} = MkBounds
    { min_x = f min_x
    , min_y = g min_y
    , max_x = f max_x
    , max_y = g max_y
    }

instance (Ord x, Ord y) => Semigroup (Bounds x y) where
  ex1 <> ex2 = MkBounds
    { min_x = min_x ex1 `min` min_x ex2
    , min_y = min_y ex1 `min` min_y ex2
    , max_x = max_x ex1 `max` max_x ex2
    , max_y = max_y ex1 `max` max_y ex2
    }

instance (Ord x, Ord y, Bounded x, Bounded y) => Monoid (Bounds x y) where
  mempty = MkBounds
    { min_x = maxBound
    , min_y = maxBound
    , max_x = minBound
    , max_y = minBound
    }

dummyBounds :: (PlotValue x, PlotValue y) => Bounds x y
dummyBounds = MkBounds
  { min_x = fromValue 0
  , min_y = fromValue 0
  , max_x = fromValue 1
  , max_y = fromValue 1
  }

-- | The `Bounds` of a single point.
pointBounds :: x -> y -> Bounds x y
pointBounds x y = MkBounds x y x y

-- | Find `Bounds` to a set of 2D points.
findBounds :: (Ord x, Ord y) => [(x, y)] -> Maybe (Bounds x y)
findBounds = \case
  [        ] -> Nothing
  (x, y):xys -> Just (foldl' acc (pointBounds x y) xys)
 where
  acc extr (x, y) = extr <> pointBounds x y

-- }}}

-- --< Internal: Tick Selection >-- {{{

data Stream a = a :~ Stream a
  deriving Functor
infixr 5 :~

ticks :: PlotValue x => Int -> x -> x -> (Rational, Rational, Rational, [x])
ticks targetNum lo hi
  | lo == hi  = ticks targetNum reducedLo increasedHi
  | otherwise = (lo', hi', tickSize,) $ [0 .. numTicks] <&> \i ->
    fromRatVal (lo' + fromInteger i * tickSize)
 where
  reducedLo   = fromRatVal $ relFloor   0.01 (loVal - 0.01)
  increasedHi = fromRatVal $ relCeiling 0.01 (hiVal + 0.01)
  loVal = toRatVal lo
  hiVal = toRatVal hi
  lo' = relCeiling tickSize loVal
  hi' = relFloor   tickSize hiVal
  tickSize = toRound $ (hiVal - loVal) / fromIntegral targetNum
  numTicks
    | tickSize == 0 = 1
    | otherwise     = round ((hi' - lo') / tickSize)

  toRatVal :: PlotValue x => x -> Rational
  toRatVal = toRational . toValue

  fromRatVal :: PlotValue x => Rational -> x
  fromRatVal = fromValue . fromRational

  toRound :: Rational -> Rational
  toRound 0  = 0
  toRound x_ = signum x_ * choose if x < 1
    then findDescending x 1 smallRounds
    else findAscending  x 1 bigRounds
   where
    x = abs x_
    choose (y, z)
      | x/y < z/x = y
      | otherwise = z
    findDescending y z        zs
      = swap $ coerce (findAscending (Down y) (Down z) (coerce zs))
    findAscending  y z0 (z :~ zs) = case compare y z of
      LT -> (z0, z)
      EQ -> (z,  z)
      GT -> findAscending y z zs

    smallRounds, bigRounds :: Stream Rational
    smallRounds = 0.5 :~ 0.25 :~ 0.1 :~ fmap (/10) smallRounds
    bigRounds   = 2.5 :~ 5    :~ 10  :~ fmap (*10) bigRounds

  relFloor, relCeiling :: RealFrac a => a -> a -> a
  relFloor   unit x
    | unit == 0 = x
    | otherwise = rfFloor   (x / unit) * unit
  relCeiling unit x
    | unit == 0 = x
    | otherwise = rfCeiling (x / unit) * unit

  rfFloor, rfCeiling :: RealFrac a => a -> a
  rfFloor   x = case properFraction x of
    (_ :: Int, y) -> x - y
  rfCeiling x = case properFraction x of
    (_ :: Int, 0) -> x
    (_       , y) -> x + (1 - y)

-- }}}

-- --< Internal: Shapes & Colours >-- {{{

shapes :: [PointShape]
shapes
  = cycle
  $ [ PointShapeCircle
{-    These don't seem to work.
    , PointShapePlus
    , PointShapeCross
    , PointShapeStar
-}
    ]
 ++ liftA2 (flip PointShapePolygon) [True, False] [3..5]

colours :: (Ord a, Floating a) => Int -> [Colour a]
colours n = [1..n] <&> \i ->
  mkColour (3 * (fromIntegral i - 0.5) / fromIntegral n)
 where
  mkColour x1
    | 0 <= x1, x1 < 1 = rgb (1 - x1)  x1       0
    | 0 <= x2, x2 < 1 = rgb  0       (1 - x2)  x2
    | 0 <= x3, x3 < 1 = rgb  x3       0       (1 - x3)
    | otherwise       = mkColour x4
   where
    x2 = x1 - 1
    x3 = x2 - 1
    x4 = x3 - 1
  rgb r g b = affineCombo (zip ([r, g, b] <&> (/mag)) [red, green, blue]) mempty
   where
    mag = sqrt (r*r + g*g + b*b)

-- }}}

-- --< Internal: SI Prefixes >-- {{{

siPrefixes :: IntMap (Text, Text)
siPrefixes = I.fromDistinctAscList
  [ ( 10, ("quetta", "Q"))
  , (  9, ("ronna" , "R"))
  , (  8, ("yotta" , "Y"))
  , (  7, ("zetta" , "Z"))
  , (  6, ("exa"   , "E"))
  , (  5, ("peta"  , "P"))
  , (  4, ("tera"  , "T"))
  , (  3, ("giga"  , "G"))
  , (  2, ("mega"  , "M"))
  , (  1, ("kilo"  , "k"))
  , (  0, (""      , "" ))
  , ( -1, ("milli" , "m"))
  , ( -2, ("micro" , "μ"))
  , ( -3, ("nano"  , "n"))
  , ( -4, ("pico"  , "p"))
  , ( -5, ("femto" , "f"))
  , ( -6, ("atto"  , "a"))
  , ( -7, ("zepto" , "z"))
  , ( -8, ("yocto" , "y"))
  , ( -9, ("ronto" , "r"))
  , (-10, ("quecto", "q"))
  ]

-- }}}

