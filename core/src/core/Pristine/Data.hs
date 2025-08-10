
-- --< Header >-- {{{

{-#
LANGUAGE
  DataKinds, RankNTypes, PatternSynonyms, UndecidableInstances,
  ViewPatterns, OverloadedStrings
#-}

{- |

Description : Data types for storing both pristine and analysed data
Copyright   : (c) 2026 L. S. Leary

Data types for storing both pristine and analysed data.

-}

-- }}}

-- --< Exports >-- {{{

module Pristine.Data (

  -- * Data
  Data(..),
  mapData,
  traverseData,
  summariseData,
  ppEstimatedData,

  -- ** Reading
  readData,
  readFunData,
  readFunDataFromField,

  -- ** `Timing`
  TimingData(..),
  mapTimingData,
  traverseTimingData,
  summariseTimingData,
  -- *** Datum
  Timing(..), time,
  Time(..),

  -- ** RTS `Stats`
  StatData(..),
  mapStatData,
  traverseStatData,
  summariseStatData,
  -- *** Datum
  Stats(..),
  statistics, (-\-),
  Space(..),

  -- ** Fun
  Fun(..),
  mapFun,
  traverseFun,
  coerceFun,

  -- ** Pre-analysis
  Stored(..), store,
  preanalyse,

  -- ** Analysis
  ppPoly,
  Estimated(..),
  estimateFunData,
  Estimator(..),
  scaleEst, ppEst,
  QuantileRange(..),
  scaleQR, ppQR,

  -- * Records
  -- ** Transparent products: t`Unit` & t`&`
  Unit(..),
  type (&)(..),
  andFst, andSnd,
  -- ** `Row`s & t`Field`s
  (:::)(Row, (:=)),
  Field(Field), fromField,

  -- * Pretty Printing
  Doc, fromDoc,
  char, text,
  line, indent,
  decimal,
  LTB.FPFormat(..),
  formatRealFloat, realFloat,

) where

-- }}}

-- --< Imports >-- {{{

-- GHC/base
import GHC.Clock (getMonotonicTimeNSec)
import GHC.TypeLits
  (Symbol, SSymbol, pattern SSymbol, KnownSymbol(..), fromSSymbol, symbolVal)
import GHC.Stats
  ( getRTSStats
  , RTSStats(allocated_bytes,copied_bytes,mutator_elapsed_ns,mutator_cpu_ns)
  )
import GHC.Generics (Generic)

-- base
import Data.Kind (Type)
import Data.Coerce (Coercible, coerce)
import Data.Proxy (Proxy(..))
import Data.Int (Int64)
import Data.String (IsString(..))
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity(..))
import Data.Bifunctor (Bifunctor(..))
import System.Mem (performMinorGC)
import System.CPUTime (getCPUTime)

-- deepseq
import Control.DeepSeq (NFData(..), rwhnf)

-- bytestring
import qualified Data.ByteString as Strict

-- text
import           Data.Text (Text)
import qualified Data.Text                        as T
import qualified Data.Text.Lazy                   as Lazy
import qualified Data.Text.Lazy.Builder           as LTB
import qualified Data.Text.Lazy.Builder.Int       as LTB
import qualified Data.Text.Lazy.Builder.RealFloat as LTB

-- vector
import qualified Data.Vector          as G
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed  as U

-- random
import System.Random (UniformRange)
import System.Random.Stateful (uniformRM)

-- filepath
import System.OsPath (OsPath)

-- directory
import System.Directory.OsPath (doesFileExist)

-- cassava
import Data.Csv
  ( FromNamedRecord(..), ToNamedRecord(..), DefaultOrdered(..)
  , FromField, ToField, namedRecord, header, (.:), (.=), decodeByName
  )

-- poly
import Data.Poly (UPoly, unPoly, scale)

-- pristine
import Pristine.Internal (Config(..), Paths(..), paths, csvErr)

-- }}}

-- --< Data >-- {{{

-- | Container-generic `Time` and `Space` data.
data Data f = MkData
  { timingData :: Maybe (TimingData f)
  , statData   :: Maybe (StatData  f)
  }

deriving instance (Show (f Time), Show (f Space)) => Show (Data f)
deriving instance (Read (f Time), Read (f Space)) => Read (Data f)
deriving instance (Eq   (f Time), Eq   (f Space)) => Eq   (Data f)
deriving instance (Ord  (f Time), Ord  (f Space)) => Ord  (Data f)

-- | Map over `Data` containers.
mapData :: (forall x. Coercible x Int64 => f x -> g x) -> Data f -> Data g
mapData fg = runIdentity . traverseData (Identity . fg)

-- | Traverse over `Data` containers.
traverseData
  :: Applicative h
  => (forall x. Coercible x Int64 => f x -> h (g x))
  -> Data f -> h (Data g)
traverseData fg MkData{timingData,statData}
  = MkData <$> traverse (traverseTimingData fg) timingData
           <*> traverse (traverseStatData  fg) statData

-- | Summarise `Data` given pretty field names.
summariseData
  :: Monoid a
  => (forall x. Coercible x Int64 => Text -> f x -> a)
  -> Data f -> a
summariseData f MkData{timingData,statData}
  = foldMap (summariseTimingData f) timingData
 <> foldMap (summariseStatData   f) statData

-- | Pretty-print `Estimated` `Data`.
ppEstimatedData
  :: Maybe Int -- ^ Precision
  -> Text      -- ^ Data name
  -> Data (Estimated v a)
  -> Text      -- ^ Variable name
  -> Doc
ppEstimatedData prec name estData var
  = line (text name) <> indent (summariseData pp estData) <> "\n"
 where
  pp field MkEstimated{estimated}
    = line ""
   <> line (text field)
   <> indent (ppEst ppQuant ppCoeff estimated var field)
   where
    ppQuant = formatRealFloat LTB.Fixed    (Just 2)
    ppCoeff = formatRealFloat LTB.Exponent prec

-- }}}

-- --< Data / Reading >-- {{{

-- | Read simple data from disk.
readData
  :: Config
  -> [Text] -- ^ Parent sections.
  -> Text   -- ^ Name.
  -> IO (Data G.Vector)
readData cnf parents name = do
  (mTimings, mStats) <- readData_ cnf parents name
  pure MkData
    { timingData  = mTimings <&> \timings -> MkTimingData
      { wall = G.map wallTime timings
      , cpu  = G.map cpuTime  timings
      }
    , statData = mStats <&> \stats_ -> MkStatData
      { allocated = G.map bytesAllocated  stats_
      , copied    = G.map bytesCopied     stats_
      , mutWall   = G.map mutatorWallTime stats_
      , mutCPU    = G.map mutatorCpuTime  stats_
      }
    }

-- | Read functional (input/output) data from disk.
readFunData
  :: FromNamedRecord a
  => Config
  -> [Text] -- ^ Parent sections.
  -> Text   -- ^ Name.
  -> IO (Data (Fun G.Vector a))
readFunData cnf parents name = do
  (mTimings, mStats) <- readData_ cnf parents name
  pure MkData
    { timingData = mTimings <&> \timings ->
      let mk = mkMk timings
      in  MkTimingData
            { wall = mk wallTime
            , cpu  = mk cpuTime
            }
    , statData  = mStats <&> \stats_ ->
      let mk = mkMk stats_
      in  MkStatData
            { allocated = mk bytesAllocated
            , copied    = mk bytesCopied
            , mutWall   = mk mutatorWallTime
            , mutCPU    = mk mutatorCpuTime
            }
    }
 where
  mkMk xys f = MkFun
    { xvals
    , yvals = G.map f yvals
    }
   where
    xvals = G.map andFst xys
    yvals = G.map andSnd xys

-- | A convenient variant of `readFunData` which reads a `Row` and extracts the value in its t`Field`.
readFunDataFromField
  :: forall f a
   . FromField a
  => Config
  -> Field f a -- ^ t`Field` to extract.
  -> [Text]    -- ^ Parent sections.
  -> Text      -- ^ Name.
  -> IO (Data (Fun G.Vector a))
readFunDataFromField cnf Field parents name
  =  mapData coerceFun
 <$> readFunData @(f ::: a) cnf parents name

readData_
  :: (FromNamedRecord t, FromNamedRecord s)
  => Config -> [Text] -> Text -> IO (Maybe (G.Vector t), Maybe (G.Vector s))
readData_ cnf parents name = do
  MkPaths{timingFile,statFile} <- paths cnf parents name
  (,) <$> readCsv timingFile <*> readCsv statFile
 where
  readCsv :: FromNamedRecord c => (OsPath, FilePath) -> IO (Maybe (G.Vector c))
  readCsv (ospath, filepath) = do
    exists <- doesFileExist ospath
    if exists then do
      bs <- Strict.readFile filepath
      case decodeByName (Strict.fromStrict bs) of
        Left msg     -> csvErr msg
        Right (_, v) -> pure (Just v)
     else do
      pure Nothing

-- }}}

-- --< Data / Timing >-- {{{

-- | Container-generic `Timing` data.
data TimingData f = MkTimingData
  { wall :: !(f Time)
  , cpu  :: !(f Time)
  }

deriving instance Show (f Time) => Show (TimingData f)
deriving instance Read (f Time) => Read (TimingData f)
deriving instance Eq   (f Time) => Eq   (TimingData f)
deriving instance Ord  (f Time) => Ord  (TimingData f)

-- | Map over `TimingData` containers.
mapTimingData
  :: (f Time -> g Time)
  -> TimingData f -> TimingData g
mapTimingData fg = runIdentity . traverseTimingData (Identity . fg)

-- | Traverse over `TimingData` containers.
traverseTimingData
  :: Applicative h
  => (f Time -> h (g Time))
  -> TimingData f -> h (TimingData g)
traverseTimingData fg MkTimingData{wall,cpu}
  = MkTimingData <$> fg wall <*> fg cpu

-- | Summarise `TimingData` given pretty field names.
summariseTimingData
  :: Monoid a
  => (Text -> f Time -> a)
  -> TimingData f -> a
summariseTimingData f MkTimingData{wall,cpu}
  = f "wall time" wall
 <> f "cpu time"  cpu

-- }}}

-- --< Data / Timing / Datum >-- {{{

-- | Measured @Timing@s.
data Timing = MkTiming
  { wallTime :: {-# UNPACK #-} !Time
  , cpuTime  :: {-# UNPACK #-} !Time
  }
 deriving stock    (Show, Read, Eq, Ord, Generic)
 deriving anyclass (ToNamedRecord, FromNamedRecord, DefaultOrdered)

instance NFData Timing where rnf = rwhnf

-- | `Time` an `IO` action.
time :: IO a -> IO (a, Timing)
time action = do
  wall1  <- getMonotonicTimeNSec
  cpu1   <- getCPUTime
  result <- action
  wall2  <- getMonotonicTimeNSec
  cpu2   <- getCPUTime
  let
    timing = MkTiming
      { wallTime =                 fromIntegral (wall2 - wall1)
      , cpuTime  = round @Double $ fromIntegral (cpu2  - cpu1 ) / 1000
      }
  pure (result, timing)

-- | @Time@ in @nanoseconds@.
newtype Time = MkTime{ nanoseconds :: Int64 }
 deriving stock   (Show, Read, Eq, Ord, Generic)
 deriving newtype
   (NFData, Num, Enum, Real, Integral, S.Storable, ToField, FromField)

-- }}}

-- --< Data / RTS Stats >-- {{{

-- | Container-generic RTS `Stats` data.
data StatData f = MkStatData
  { allocated :: !(f Space)
  , copied    :: !(f Space)
  , mutWall   :: !(f Time)
  , mutCPU    :: !(f Time)
  }

deriving instance (Show (f Space), Show (f Time)) => Show (StatData f)
deriving instance (Read (f Space), Read (f Time)) => Read (StatData f)
deriving instance (Eq   (f Space), Eq   (f Time)) => Eq   (StatData f)
deriving instance (Ord  (f Space), Ord  (f Time)) => Ord  (StatData f)

-- | Map over `StatData` containers.
mapStatData
  :: (forall x. Coercible x Int64 => f x -> g x)
  -> StatData f -> StatData g
mapStatData fg = runIdentity . traverseStatData (Identity . fg)

-- | Traverse over `StatData` containers.
traverseStatData
  :: Applicative h
  => (forall x. Coercible x Int64 => f x -> h (g x))
  -> StatData f -> h (StatData g)
traverseStatData fg MkStatData{allocated,copied,mutWall,mutCPU}
  = MkStatData <$> fg allocated <*> fg copied
                <*> fg mutWall   <*> fg mutCPU

-- | Summarise `StatData` given pretty field names.
summariseStatData
  :: Monoid a
  => (forall x. Coercible x Int64 => Text -> f x -> a)
  -> StatData f -> a
summariseStatData f MkStatData{allocated,copied,mutWall,mutCPU}
  = f "bytes allocated"   allocated
 <> f "bytes copied"      copied
 <> f "mutator wall time" mutWall
 <> f "mutator cpu time"  mutCPU

-- }}}

-- --< Data / RTS Stats / Datum >-- {{{

-- | Collected RTS @Stats@.
data Stats = MkStats
  { bytesAllocated  :: {-# UNPACK #-} !Space
  , bytesCopied     :: {-# UNPACK #-} !Space
  , mutatorWallTime :: {-# UNPACK #-} !Time
  , mutatorCpuTime  :: {-# UNPACK #-} !Time
  }
 deriving stock    (Show, Read, Eq, Ord, Generic)
 deriving anyclass (ToNamedRecord, FromNamedRecord, DefaultOrdered)

instance NFData Stats where rnf = rwhnf

-- | Take one set of `Stats` relative to another.
(-\-) :: Stats -> Stats -> Stats
s -\- t = MkStats
  { bytesAllocated  = bytesAllocated  s - bytesAllocated  t
  , bytesCopied     = bytesCopied     s - bytesCopied     t
  , mutatorWallTime = mutatorWallTime s - mutatorWallTime t
  , mutatorCpuTime  = mutatorCpuTime  s - mutatorCpuTime  t
  }

-- | Collect RTS `Stats` for an `IO` action.
statistics :: IO a -> IO (a, Stats)
statistics act = do
  s1 <- getStats
  x  <- act
  s2 <- getStats
  pure (x, s2 -\- s1)
 where
  getStats = do
    performMinorGC -- Updates RTSStats
    s <- getRTSStats
    pure MkStats
      { bytesAllocated  = MkSpace (fromIntegral $ allocated_bytes    s)
      , bytesCopied     = MkSpace (fromIntegral $ copied_bytes       s)
      , mutatorWallTime = MkTime  (               mutator_elapsed_ns s)
      , mutatorCpuTime  = MkTime  (               mutator_cpu_ns     s)
      }

-- | @Space@ in @bytes@.
newtype Space = MkSpace{ bytes :: Int64 }
 deriving stock   (Show, Read, Eq, Ord, Generic)
 deriving newtype
   (NFData, Num, Enum, Real, Integral, S.Storable, ToField, FromField)

-- }}}

-- --< Data / Fun >-- {{{

-- | Container-generic input/output data.
data Fun v x y = MkFun
  { xvals :: !(v x)
  , yvals :: !(v y)
  }
 deriving (Show, Read, Eq, Ord, Functor)

-- | Map over `Fun` containers and elements.
mapFun
  :: (u w -> v x) -> (u y -> v z) {- ^ -}
  -> Fun u w y -> Fun v x z
mapFun wx yz = runIdentity . traverseFun (Identity . wx) (Identity . yz)

-- | Traverse over `Fun` containers and elements.
traverseFun
  :: Applicative f
  => (u w -> f (v x)) -> (u y -> f (v z)) {- ^ -}
  -> Fun u w y -> f (Fun v x z)
traverseFun wx yz (MkFun xvals yvals) = MkFun <$> wx xvals <*> yz yvals

-- | Coerce `Fun` containers and elements.
coerceFun
  :: (Coercible (u w) (v x), Coercible (u y) (v z))
  => Fun u w y -> Fun v x z {- ^ -}
coerceFun = mapFun coerce coerce

-- }}}

-- --< Data / Pre-analysis >-- {{{

-- | /pristine-analysis/ deals only in `S.Storable` `S.Vector`s of `Double`s.
--   However, converting all our types to `Double` is a recipe for confusion.
--   `Stored` remembers the original type of our elements to prevents mixups.
newtype Stored (a :: Type) = MkStored{ unMkStored :: S.Vector Double }
 deriving (Show, Read, Eq, Ord)

-- | Given a conversion from @a@ to `Double`, @store@ a generic @`G.Vector` a@.
store :: (a -> Double) -> G.Vector a -> Stored a
store f = MkStored . S.convert . G.map f

-- | Given a conversion from @a@ to `Double`, `store` `Data` read from disk for analysis.
preanalyse :: (a -> Double) -> Data (Fun G.Vector a) -> Data (Fun Stored a)
preanalyse unx
  = mapData
  $ mapFun (store unx) (store (fromIntegral @Int64 . coerce))

-- }}}

-- --< Data / Analysis >-- {{{

-- | Given a pretty printer for the coefficients, pretty-print a polynomial with the given variable name.
ppPoly :: (U.Unbox a, Num a, Ord a) => (a -> Doc) -> UPoly a -> Text -> Doc
ppPoly ppCoeff p var = go (U.length coeffs - 1)
 where
  coeffs = unPoly p
  go -1 = ""
  go  n = ppSign
       <> ppCoeff (abs coeff)
       <> ppPower (" " <> var) n
       <> go (n - 1)
   where
    coeff = coeffs U.! n
    ppSign
      | n == U.length coeffs - 1 = if signum coeff < 0 then  "-"  else ""
      | otherwise                = if signum coeff < 0 then " - " else " + "
    ppPower _    0 = mempty
    ppPower base 1 = text base
    ppPower base k = text base <> "^" <> decimal k

-- | Container-generic `Fun`ctional data accompanied by its `Estimator`.
data Estimated v x y = MkEstimated
  { sample    :: !(Fun v x y)
  , estimated :: !Estimator
  }
 deriving (Show, Eq, Ord)

-- | Map an estimation function over `Fun`ctional `Data`.
estimateFunData
  :: (Fun S.Vector Double Double -> Estimator)
  -> Data (Fun Stored a) -> Data (Estimated Stored a)
estimateFunData f = mapData \sample -> MkEstimated
  { sample
  , estimated = f (coerceFun sample)
  }

-- | The estimated median and any number of quantile ranges.
data Estimator = MkEstimator
  { estMedian    :: !(UPoly Double)
  , estQuantiles :: ![QuantileRange]
  }
 deriving (Show, Eq, Ord)

-- | Scale an `Estimator`.
scaleEst :: Double -> Estimator -> Estimator
scaleEst lam MkEstimator{estMedian,estQuantiles} = MkEstimator
  { estMedian    =      scale 0 lam  estMedian
  , estQuantiles = map (scaleQR lam) estQuantiles
  }

-- | Pretty-print an `Estimator`.
ppEst
  :: (Double -> Doc) -- ^ Quantile pretty printer.
  -> (Double -> Doc) -- ^ Coefficient pretty printer.
  -> Estimator
  -> Text            -- ^ Name of the variable on the x-axis.
  -> Text            -- ^ Name of the variable on the y-axis.
  -> Doc
ppEst ppQuant ppCoeff MkEstimator{estMedian,estQuantiles} xvar yvar
  = line ("median " <> text yvar <> ": " <> ppPoly ppCoeff estMedian xvar)
 <> foldMap (\qr -> line (ppQR ppQuant ppCoeff qr xvar yvar)) estQuantiles

-- | An inter-quantile range symmetric about the median.
data QuantileRange = MkQuantileRange
    -- | \( r_p \in \left( 0, \frac{1}{2} \right) \)
  { probabilityRadius :: !Double
  , lowerQuantile     :: !(UPoly Double) -- ^ \( Q_{\frac{1}{2} - r_p} \)
  , upperQuantile     :: !(UPoly Double) -- ^ \( Q_{\frac{1}{2} + r_p} \)
  }
 deriving (Show, Eq, Ord)

-- | Scale a `QuantileRange`.
scaleQR :: Double -> QuantileRange -> QuantileRange
scaleQR lam qr@MkQuantileRange{lowerQuantile,upperQuantile} = qr
  { lowerQuantile = scale 0 lam lowerQuantile
  , upperQuantile = scale 0 lam upperQuantile
  }

-- | Pretty-print a `QuantileRange`.
ppQR
  :: (Double -> Doc) -- ^ Quantile pretty printer.
  -> (Double -> Doc) -- ^ Coefficient pretty printer.
  -> QuantileRange
  -> Text            -- ^ Name of the variable on the x-axis.
  -> Text            -- ^ Name of the variable on the y-axis.
  -> Doc
ppQR ppQuant ppCoeff qr xvar yvar
  = ppQuant (0.5 - probabilityRadius) <> "–"
 <> ppQuant (0.5 + probabilityRadius) <> ": "
 <> ppPoly ppCoeff lowerQuantile xvar <> " ≤ " <> text yvar <> " ≤ "
 <> ppPoly ppCoeff upperQuantile xvar
 where !MkQuantileRange{probabilityRadius,lowerQuantile,upperQuantile} = qr

-- }}}

-- --< Records / Unit & (&) >-- {{{

-- | The t`Unit` to t`&`; invisible in CSV.
data Unit = Unit
 deriving (Show, Read, Eq, Ord, Enum, Generic)

instance NFData Unit where
  rnf Unit = ()

instance FromNamedRecord Unit where
  parseNamedRecord _ = pure Unit

instance ToNamedRecord Unit where
  toNamedRecord Unit = mempty

instance DefaultOrdered Unit where
  headerOrder _ = mempty

-- | Strict pairs with transparent CSV instances.
data a & b = !a :& !b
 deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable, Generic)
infixr 1 :&

instance Bifunctor (&) where
  bimap f g (x :& y) = f x :& g y

instance (NFData a, NFData b) => NFData (a & b) where
  rnf (x :& y) = rnf x `seq` rnf y

instance (FromNamedRecord a, FromNamedRecord b) => FromNamedRecord (a & b) where
  parseNamedRecord m = liftA2 (:&) (parseNamedRecord m) (parseNamedRecord m)

instance (ToNamedRecord a, ToNamedRecord b) => ToNamedRecord (a & b) where
  toNamedRecord (x :& y) = toNamedRecord x <> toNamedRecord y

instance (DefaultOrdered a, DefaultOrdered b) => DefaultOrdered (a & b) where
  headerOrder ~(x :& y) = headerOrder x <> headerOrder y

-- | `fst` for t`&`.
andFst :: a & b -> a
andFst (x :& _) = x

-- | `snd` for t`&`.
andSnd :: a & b -> b
andSnd (_ :& y) = y

-- }}}

-- --< Records / Rows & Fields >-- {{{

-- | @"foo" ::: a@ is the type of /rows/ with field name @foo@ describing values of type  @a@.
type (:::) :: Symbol -> Type -> Type
newtype field ::: a = Row a
 deriving stock   (Show, Read, Eq, Ord, Foldable, Traversable, Functor, Generic)
 deriving newtype (NFData, S.Storable, Num, Fractional, IsString)
 deriving         (Applicative, Monad)
  via Identity

instance (KnownSymbol f, FromField a) => FromNamedRecord (f ::: a) where
  parseNamedRecord m = Row <$> m .: fieldName (Proxy @f)

instance (KnownSymbol f, ToField a) => ToNamedRecord (f ::: a) where
  toNamedRecord (Row a) = namedRecord [fieldName (Proxy @f) .= a]

instance KnownSymbol f => DefaultOrdered (f ::: a) where
  headerOrder _ = header [fieldName (Proxy @f)]

instance UniformRange a => UniformRange (f ::: a) where
  uniformRM (Row x, Row y) g = Row <$> uniformRM (x, y) g

viewRow :: KnownSymbol f => f ::: a -> (Field f a, a)
viewRow (Row x) = (Field, x)

pattern (:=) :: KnownSymbol f => Field f a -> a -> f ::: a
pattern field := x <- (viewRow -> (field, x))
  where _     := x  =      Row            x

fieldName :: forall {proxy} f. KnownSymbol f => proxy f -> Strict.ByteString
fieldName = fromString . symbolVal

-- | Value-level representation of @Field@ names, tied to the type they describe.
type role Field    nominal   representational
type      Field :: Symbol -> Type          -> Type
newtype   Field    f         a              = MkField (SSymbol f)

{-# COMPLETE Field #-}
pattern Field :: () => KnownSymbol f => Field f a
pattern Field = MkField SSymbol

instance Show (Field f a) where
  showsPrec p (MkField s)
    = showParen (p >= 11)
    $ showString "Field @"
    . showsPrec 11 (fromSSymbol s)

instance NFData (Field f a) where
  rnf (MkField s) = rnf (fromSSymbol s)

-- | Render a t`Field` to your string type of choice.
fromField :: IsString s => Field f a -> s
fromField (MkField s) = fromString (fromSSymbol s)

-- }}}

-- --< Pretty Printing >-- {{{

-- | A simple @Doc@ument target for pretty printing.
newtype Doc = MkDoc{ unDoc :: Int -> LTB.Builder }
  deriving newtype (Semigroup, Monoid)

instance IsString Doc where
  fromString s = MkDoc \_ -> fromString s

-- | Render `Doc` to lazy `Lazy.Text`.
fromDoc :: Doc -> Lazy.Text
fromDoc d = LTB.toLazyText (unDoc d 0)

-- | Create a `Doc` from a `Char`.
char :: Char -> Doc
char c = MkDoc \_ -> LTB.singleton c

-- | Create a `Doc` from strict `Text`.
text :: Text -> Doc
text t = MkDoc \_ -> LTB.fromText t

-- | Declare a @line@.
line :: Doc -> Doc
line d = MkDoc \level ->
  LTB.fromText (T.replicate (level * 2) " ")
    <> unDoc d level <> LTB.singleton '\n'

-- | @indent@ a `Doc` one level.
indent :: Doc -> Doc
indent d = MkDoc \level -> unDoc d (level + 1)

-- | Pretty print an `Integral` number in @decimal@.
decimal :: Integral a => a -> Doc
decimal x = MkDoc \_ -> LTB.decimal x

-- | Pretty print a `RealFloat` with formatting at a given precision.
formatRealFloat :: RealFloat a => LTB.FPFormat -> Maybe Int -> a -> Doc
formatRealFloat fmt mprec x = MkDoc \_ -> LTB.formatRealFloat fmt mprec x

-- | Default pretty printer for `RealFloat`s.
--
-- @
--   realFloat = `formatRealFloat` `LTB.Generic` `Nothing`
-- @
realFloat :: RealFloat a => a -> Doc
realFloat = formatRealFloat LTB.Generic Nothing

-- }}}

