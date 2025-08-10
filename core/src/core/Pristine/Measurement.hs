
-- --< Header >-- {{{

{-# LANGUAGE GADTs, DataKinds, RankNTypes #-}

{- |

Description : Pristine measurment
Copyright   : (c) 2026 L. S. Leary

Pristine measurment.

-}

-- }}}

-- --< Exports >-- {{{

module Pristine.Measurement (

  -- * `Benchmark`
  Benchmark,
  run, runWith,
  -- ** `Config`
  Config(..), defConfig,
  -- ** `Ensemble`
  Ensemble,
  (=:=), section,
  local, with,
  -- *** Functions
  Arg, argument,
  -- **** `Gen`
  Gen(..), uniform,
  -- *** Introspection
  Forest, Rose(..),
  forest, list,

  -- * `Benchmarkable`
  Benchmarkable,
  -- ** `Action`
  Action, io_,
  -- *** Conveniences
  io, io',
  ($$), (!$),
  (~$$), ($$$), (!$$),

  -- * `Resource`
  Resource, res_,
  -- ** Conveniences
  res, res',
  env_, env, env',
  schedule,
  ultimately,

) where

-- }}}

-- --< Imports >-- {{{

-- base
import Data.Proxy (Proxy(..))
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid (Ap(..))
import Data.Foldable (traverse_)
import Data.Functor (void, ($>), (<&>))
import Data.Bifunctor (Bifunctor(..))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Exception (evaluate, bracket, finally)
import Control.Concurrent (threadDelay)
import System.Mem (performMajorGC)
import System.IO
  (Handle, withFile, stdout, stderr, hPutStrLn, hClose, IOMode(..), hFlush)

-- primitive
import Control.Monad.Primitive (RealWorld)
import Data.Primitive.PrimVar (PrimVar, newPrimVar, readPrimVar, writePrimVar)
import Data.Primitive.SmallArray
  ( SmallArray, smallArrayFromList, createSmallArray, sizeofSmallArray
  , indexSmallArray, copySmallArray
  )

-- deepseq
import Control.DeepSeq (NFData(..), force, rwhnf)

-- bytestring
import qualified Data.ByteString      as Strict
import qualified Data.ByteString.Lazy as Lazy

-- text
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)

-- transformers
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.State.Strict (state)

-- random
import System.Random (newStdGen, UniformRange, uniformR)
import System.Random.Stateful (runStateGenT, uniformRM)

-- filepath
import System.OsPath (OsPath)

-- directory
import System.Directory.OsPath (doesFileExist)

-- cassava
import Data.Csv
  ( FromNamedRecord(..), ToNamedRecord(..), DefaultOrdered(..)
  , encodeDefaultOrderedByName, EncodeOptions(..), defaultEncodeOptions
  )
import Data.Csv.Incremental (encodeDefaultOrderedByNameWith, encodeNamedRecord)
import qualified Data.Csv.Streaming as Streaming

-- ansi-terminal
import System.Console.ANSI
  ( hideCursor, showCursor, clearScreen
  , setCursorPosition, setCursorColumn, cursorDownLine
  )

-- pristine
import Pristine.Internal
import Pristine.Data
  (Timing(..), time, Stats(..), statistics, Unit(..), type (&)(..))

-- }}}

-- --< Benchmark >-- {{{

-- | A @Benchmark@ is just an `Ensemble` of `Config`ured `Benchmarkable`s.
type Benchmark = Ensemble Config Benchmarkable

-- | Run a `Benchmark` with default configuration.
run :: Benchmark -> IO ()
run = runWith defConfig

-- | Run a `Benchmark` with the supplied configuration.
runWith :: Config -> Benchmark -> IO ()
runWith cnf benchmark = do
  Unknown rtsStatsSupport <- checkRtsStatsSupport
  withResource (targetsFrom rtsStatsSupport cnf benchmark) \targets0 -> do
    stdg <- newStdGen
    (void . runStateGenT stdg) \g ->
      while (> 0) sizeofSmallArray targets0 \targets n -> do
        i <- uniformRM (0, n - 1) g
        case indexSmallArray targets i of
          target@MkTarget{config,benchmarkable,status
                         ,generator,timings,rtsStats,progressAt} -> do
            let sched = samples config
            work <- readPrimVar status
            if work >= sched
             then lift do
              conclude target $> targets \\ i
             else do
              x <- state (next generator)
              lift do
                (timing, supStats) <- measureFun
                  rtsStatsSupport config benchmarkable x
                Lazy.hPut timings (encodeNamedRow (x :& timing))
                mfor rtsStats \h ->
                  mfor supStats \s ->
                    Lazy.hPut h (encodeNamedRow (x :& s))
                let !work' = work + 1
                writePrimVar status work'
                report progressAt work' sched
                pure targets

report :: (Int, Int) -> Int -> Int -> IO ()
report (line, col) work sched = do
  setCursorPosition line col
  putStr (show work ++ "/" ++ show sched)
  hFlush stdout

-- }}}

-- --< Benchmark / Config >-- {{{

-- | Default `Config`.
defConfig :: Config
defConfig = MkConfig
  { delay     = 10_000
  , samples   = 200
  , directory = ""
  }

-- }}}

-- --< Benchmark / Ensemble >-- {{{

-- | @Ensemble@s of named and sectioned @p@s with associated localised @c@s.
--   @Ensemble@ elements may depend on randomly generated arguments and scoped resources, but the structure itself cannot.
data Ensemble c p where
  Mempty    ::                                          Ensemble c p
  (:<>)     :: Ensemble c p   -> Ensemble c       p  -> Ensemble c p
  (:==)     :: Text           ->                  p  -> Ensemble c p
  Section   :: Text           -> Ensemble c       p  -> Ensemble c p
  Local     :: (c -> c)       -> Ensemble c       p  -> Ensemble c p
  With      :: Resource r     -> Ensemble c (r -> p) -> Ensemble c p
  Function  :: Arg x => Gen x -> Ensemble c (x -> p) -> Ensemble c p

deriving instance Functor (Ensemble c)

instance Semigroup (Ensemble c p) where (<>)   = (:<>)
instance Monoid    (Ensemble c p) where mempty = Mempty

-- | Create a singleton `Ensemble` from a named value.
(=:=) :: Text -> p -> Ensemble c p
(=:=) = (:==)

infix 7 =:=

-- | Enclose an `Ensemble` into a named @section@.
section :: Text -> Ensemble c p -> Ensemble c p
section = Section

-- | Apply a local transformation to @c@.
local :: (c -> c) -> Ensemble c p -> Ensemble c p
local = Local

-- | Provide a scoped @`Resource` r@ to the underlying `Ensemble`.
with :: Resource r -> Ensemble c (r -> p) -> Ensemble c p
with = With

-- }}}

-- --< Benchmark / Ensemble / Functions >-- {{{

-- | Provide arguments to an underlying `Ensemble` by way of a `Gen`erator.
argument :: Arg x => Gen x -> Ensemble c (x -> p) -> Ensemble c p
argument = Function

-- | Generate @a@s @uniform@ly within the given range.
uniform :: UniformRange a => (a, a) -> Gen a
uniform t = MkGen{ next = uniformR t }

-- }}}

-- --< Benchmark / Ensemble / Introspection >-- {{{

-- | @Forest@s of `Rose` trees.
type Forest a b = [Rose a b]

-- | @Rose@ trees with @a@-nodes and @b@-leaves.
--   Provided for introspection of `Ensemble`s via `forest`.
data Rose a b = Leaf b | Node a (Forest a b)
 deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

-- | Introspect the `section`s, names and corresponding @c@s of an `Ensemble`.
forest :: c -> Ensemble c p -> Forest Text (c, Text)
forest c
  = tidy
  . introspect
      (\f name -> [Leaf (f c, name)])
      (\sname l -> [Node sname l])
 where
  tidy :: Eq a => Forest a b -> Forest a b
  tidy = foldr integrate [] . stablePartitionBy root
   where
    integrate (Leaf b    :| _ ) rest = Leaf b:rest
    integrate (Node n cs :| ns) rest
      | null children = rest
      | otherwise     = Node n children:rest
     where
      children = tidy (cs ++ foldr denode [] ns)
      denode (Node _ ds) = (ds ++)
      denode  _          =  id
    root (Node m _) (Node n _) = m == n
    root  _          _         = False

-- | Given a base @c@, list the parents, names and localised @c@s of an `Ensemble`.
list :: c -> Ensemble c p -> [(c, [Text], Text)]
list c0 = foldMap (list_ id) . forest c0
 where
  list_ parents = \case
    Leaf (c, name)      -> [(c, parents [], name)]
    Node sname children -> list_ (parents . (sname:)) =<< children

introspect
  :: forall b c p
   . Monoid b
  => ((c -> c) -> Text -> b)
  -> (Text -> b -> b)
  -> Ensemble c p -> b
introspect singleton_ section_ = go id
 where
  go :: (c -> c) -> Ensemble c q -> b
  go f = \case
    Mempty           -> mempty
    e1 :<> e2        -> go f e1 <> go f e2
    name :== _       -> singleton_ f name
    Section  sname e -> section_ sname
                      $ go      f  e
    Local    g e     -> go (g . f) e
    With     _ e     -> go      f  e
    Function _ e     -> go      f  e

summarised :: Rose a b -> Rose (Int, a) (Int, b)
summarised = go 0
 where
  go !l = \case
    Leaf y    -> Leaf (l, y)
    Node x cs -> Node (1 + sum (map height cs'), x) cs'
     where cs' = go (l + 1) <$> cs

height :: Rose (Int, a) b -> Int
height (Leaf        _) = 1
height (Node (n, _) _) = n

findPosition
  :: (Eq a, Eq b)
  => Forest (Int, a) (Int, b) -> [a] -> b -> Maybe (Int, Int)
findPosition = go 0
 where
  go !line0 f0 path y0 = case path of
    [  ]   -> findLeaf line0 f0 y0
     where
      findLeaf !line f z = case f of
        [      ]                   -> Nothing
        Leaf (level, y):_ | y == z -> Just (line, level)
        r              :f'         -> findLeaf (line + height r) f' z
    x0:xs0 -> findNode line0 f0 x0 xs0 y0
     where
      findNode !line f x xs y = case f of
        [      ]                  -> Nothing
        Node (_, z) cs:_ | z == x -> go (line + 1) cs xs y
        r             :f'         -> findNode (line + height r) f' x xs y

-- }}}

-- --< Benchmarkable >-- {{{

-- | A @Benchmarkable@ entity is simply an `Action` with access to local per-run `Resource`s.
type Benchmarkable = Resource Action

measureFun
  :: NFData a
  => Support s -> Config
  -> (a -> Benchmarkable)
  ->  a -> IO (Timing, Supported s Stats)
measureFun rtsStatsSupport cnf benchmarkable rec = do
  arguments <- evaluate (force rec)
  measure rtsStatsSupport cnf (benchmarkable arguments)

measure
  :: Support s -> Config -> Benchmarkable
  -> IO (Timing, Supported s Stats)
measure rtsStatsSupport MkConfig{delay} bm = withResource bm \act -> do
  act' <- evaluate act
  performMajorGC
  threadDelay delay
  let timed = snd <$> time (perform act')
  case rtsStatsSupport of
    IsDisabled ->            timed <&>   (, Unsupported)
    IsEnabled  -> statistics timed <&> second Supported

-- }}}

-- --< Benchmarkable / Action >-- {{{

-- | Simple @Action@s; these are ultimately what `Benchmark`s measure.
newtype Action = MkAction{ perform :: IO () }

-- Though `(<>) @(IO ())` is usually fine, it does have a small performance
-- penalty relative to `(>>)`. To avoid any effect on our measurements, we
-- ensure `Action`s are combined with the latter. In fact, this instance is the
-- principle motivation for the `Action` newtype in the first place.
instance Semigroup Action where
  MkAction io1 <> MkAction io2 = MkAction (io1 >> io2)

instance Monoid Action where
  mempty = MkAction (pure ())

-- | Execute `IO`.
io_ :: IO () -> Action
io_ = MkAction

-- | Execute `IO`, evaluating the result to weak head normal form.
io :: IO a -> Action
io act = io_ do
  x <- act
  evaluate (rwhnf x)

-- | Execute `IO`, evaluating the result to normal form.
io' :: NFData a => IO a -> Action
io' act = io_ do
  x <- act
  evaluate (rnf x)

-- | Evaluate an application to weak head normal form.
($$) :: (a -> b) -> a -> Action
f $$ x = pure . f $$$ x

-- | Evaluate an application to normal form.
(!$) :: NFData b => (a -> b) -> a -> Action
f !$ x = pure . f !$$ x

-- | Execute an application.
(~$$) :: (a -> IO ()) -> a -> Action
p ~$$ !x = io_ (p x)

-- | Execute an application, evaluating its result to weak head normal form.
($$$) :: (a -> IO b) -> a -> Action
p $$$ !x = io (p x)

-- | Execute an application, evaluating its result to normal form.
(!$$) :: NFData b => (a -> IO b) -> a -> Action
p !$$ !x = io' (p x)

infix 1 $$, !$, ~$$, $$$, !$$

-- }}}

-- --< Resource >-- {{{

-- | A `Monad` for accessing scoped @Resource@s.
data Resource r = MkResource{ withResource :: forall x. (r -> IO x) -> IO x }
 deriving  Functor
 deriving (Semigroup, Monoid)
  via Ap Resource r

instance Applicative Resource where
  pure x = MkResource \k -> k x
  liftA2 f (MkResource withx) (MkResource withy) = MkResource \k ->
    withx \x ->
    withy \y ->
    k (f x y)

instance Monad Resource where
  MkResource withx >>= k0 = MkResource \k1 ->
    withx \x ->
    withResource (k0 x) k1

instance MonadIO Resource where
  liftIO act = MkResource (act >>=)

-- | Introduce a new scoped resource.
res_ :: (forall x. (a -> IO x) -> IO x) -> Resource a
res_ = MkResource

-- | Introduce a new scoped resource.
--   Evaluated to weak head normal form before use.
res
  :: (forall x. (res -> IO x) -> IO x)
  -> Resource res
res introduce = res_ introduce >>= liftIO . evaluate

-- | Introduce a new scoped resource.
--   Evaluated to normal form before use.
res'
  :: NFData res
  => (forall x. (res -> IO x) -> IO x)
  -> Resource res
res' introduce = res_ introduce >>= liftIO . evaluate . force

-- | Introduce a new value to the environment.
env_ :: IO env -> Resource env
env_ getEnv = res_ (getEnv >>=)

-- | Introduce a new value to the environment.
--   Evaluated to weak head normal form before use.
env :: IO env -> Resource env
env getEnv = res (getEnv >>=)

-- | Introduce a new value to the environment.
--   Evaluated to normal form before use.
env' :: NFData env => IO env -> Resource env
env' getEnv = res' (getEnv >>=)

-- | Schedule `IO` to run when leaving the scope successfully.
schedule :: IO () -> Resource ()
schedule act = res_ \use -> use () <* act

-- | Schedule `IO` to run when leaving the scope, successfully or otherwise.
ultimately :: IO () -> Resource ()
ultimately act = res_ \use -> use () `finally` act

-- }}}

-- --< Internal: Target >-- {{{

data Target (s :: SupportKind) where
  MkTarget
    :: Arg a
    => { config        :: !Config
       , generator     :: !(Gen a)
       , benchmarkable :: !(a -> Benchmarkable)
       , status        :: !(PrimVar RealWorld Int)
       , timings       :: !Handle
       , rtsStats      :: !(Supported s Handle)
       , progressAt    :: !(Int, Int)
       }
    -> Target s

conclude :: Target s -> IO ()
conclude MkTarget{timings,rtsStats} = do
  hClose timings
  foldMap hClose rtsStats

targetsFrom
  :: Support s -> Config -> Benchmark
  -> Resource (SmallArray (Target s))
targetsFrom rtsStatsSupport0 cnf0 bm0 = do
  sansCursor
  liftIO do
    clearScreen
    setCursorPosition 0 0
    traverse_ (display 0) ops
  let !totalHeight = sum (map height ops)
  schedule do
    setCursorPosition totalHeight 0
    putStrLn "Done.\n"
  smallArrayFromList
    <$> targetsFrom_ rtsStatsSupport0 cnf0 (const <$> bm0) (pure Unit) []
 where
  !ops = summarised . fmap snd <$> forest cnf0 bm0
  targetsFrom_
    :: Arg a
    => Support s -> Config -> Ensemble Config (a -> Benchmarkable)
    -> Gen a -> [Text] -> Resource [Target s]
  targetsFrom_ rtsStatsSupport config bm1 generator rparents = case bm1 of
    Mempty                 -> pure []
    bm2 :<> bm3
      -> targetsFrom_ rtsStatsSupport config bm2 generator rparents
      <> targetsFrom_ rtsStatsSupport config bm3 generator rparents
    name :== benchmarkable -> do
      let parents = reverse rparents
          progressAt = case findPosition ops parents name of
            Nothing            -> die "targetsFrom" "bug"
            Just (line, level) -> (line, 2 * level + T.length name + 2)
      MkPaths{timingFile,statFile} <- liftIO (paths config parents name)
      (timeWork, timings ) <- open generator (Proxy @Timing) timingFile
      (statWork, rtsStats) <- case rtsStatsSupport of
        IsDisabled -> pure (timeWork, Unsupported)
        IsEnabled  -> second Supported
                  <$> open generator (Proxy @Stats) statFile
      let workDone = timeWork `min` statWork
      status <- liftIO do
        report progressAt workDone (samples config)
        newPrimVar workDone
      pure
        [ MkTarget{config,benchmarkable,status,generator
                  ,timings,rtsStats,progressAt}
        ]
    Section sname bm2      ->
      targetsFrom_ rtsStatsSupport config bm2 generator (sname:rparents)
    Local f   bm2          ->
      targetsFrom_ rtsStatsSupport (f config) bm2 generator rparents
    With rsrc bm2          -> do
      r <- rsrc
      targetsFrom_ rtsStatsSupport config (bm2 <&> ($ r)) generator rparents
    Function genx bm2      -> do
      let gen' = liftA2 (:&) genx generator
          adjust f (x :& y) = f x y
      targetsFrom_ rtsStatsSupport config (adjust <$> bm2) gen' rparents
   where
    open
      :: forall {proxy1} {proxy2} x a
       . (CSV x, CSV a)
      => proxy1 x -> proxy2 a -> (OsPath, FilePath)
      -> Resource (Int, Handle)
    open x a (ospath, filepath) = do
      workDone <- liftIO do
        exists <- doesFileExist ospath
        if not exists then do
          let hdr = encodeHeader x a
          Lazy.writeFile filepath hdr
          pure 0
         else samplesOnDisk
      handle <- res (withFile filepath AppendMode)
      pure (workDone, handle)
     where
      samplesOnDisk = withFile filepath ReadMode \csvFile -> do
        bs <- Lazy.hGetContents csvFile
        case Streaming.decodeByName @(x & a) bs of
          Left msg      -> csvErr msg
          Right (_, rs) -> num 0 rs
       where
        num !n = \case
          Streaming.Nil mErrMsg remaining -> do
            traverse_ csvErr mErrMsg
            unless (Lazy.null remaining) do
              let t = T.unpack (decodeUtf8 (Strict.toStrict remaining))
              hPutStrLn stderr ("Trailing input in " ++ filepath ++ ": " ++ t)
            pure n
          Streaming.Cons esa rs           -> do
            () <- either csvErr mempty esa
            num (n + 1) rs

type CSV a = (FromNamedRecord a, ToNamedRecord a, DefaultOrdered a)

die :: String -> String -> a
die name msg = error ("Pristine.Measure." ++ name ++ ": " ++ msg)

sansCursor :: Resource ()
sansCursor = res_ $ bracket hideCursor (const showCursor)

display :: Int -> Rose (a, Text) (b, Text) -> IO ()
display !col r = do
  setCursorColumn col
  case r of
    Leaf (_,  name)    -> do
      putStr (T.unpack  name ++ ": ")
      cursorDownLine 1
    Node (_, sname) cs -> do
      putStr (T.unpack sname)
      cursorDownLine 1
      let col' = col + 2
      traverse_ (display col') cs

-- }}}

-- --< Internal: Utils >-- {{{

stablePartitionBy :: (a -> a -> Bool) -> [a] -> [NonEmpty a]
stablePartitionBy  _  [    ] = []
stablePartitionBy (~) (x:ys) = case partition (x ~) ys of
  (xs, zs) -> (x:|xs):stablePartitionBy (~) zs

mfor :: (Foldable f, Monoid b) => f a -> (a -> b) -> b
mfor = flip foldMap

encodeHeader
  :: forall {proxy1} {proxy2} x a
   . ( ToNamedRecord x, DefaultOrdered x
     , ToNamedRecord a, DefaultOrdered a
     )
  => proxy1 x -> proxy2 a -> Lazy.ByteString
encodeHeader _ _ = encodeDefaultOrderedByName @(x & a) []

encodeNamedRow
  :: (ToNamedRecord a, DefaultOrdered a)
  => a -> Lazy.ByteString
encodeNamedRow rec = encodeDefaultOrderedByNameWith opts encBldr
 where
  opts    = defaultEncodeOptions{ encIncludeHeader = False }
  encBldr = encodeNamedRecord rec

while :: Monad m => (b -> Bool) -> (a -> b) -> a -> (a -> b -> m a) -> m a
while p testee x0 body = loop x0
 where
  loop x = do
    let y = testee x
    if p y then do
      x' <- body x y
      loop x'
     else do
      pure x

(\\) :: SmallArray a -> Int -> SmallArray a
sa \\ idx
  | n == 0    = sa
  | otherwise = createSmallArray (n - 1) (indexSmallArray sa 0) \sma -> do
    copySmallArray sma 0   sa  0         idx
    copySmallArray sma idx sa (idx + 1) (n - 1 - idx)
 where
  n = sizeofSmallArray sa

-- }}}

