{-# LANGUAGE GADTs, DataKinds #-}

module Pristine.Internal where

-- --< Imports >-- {{{

-- GHC/base
import GHC.Stats (getRTSStatsEnabled)

-- base
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity(..))
import Control.Exception (Exception, throwIO)

-- deepseq
import Control.DeepSeq (NFData(..))

-- text
import           Data.Text (Text)
import qualified Data.Text as T

-- transformers
import Control.Monad.Trans.State.Strict (StateT(..), State)

-- random
import System.Random (StdGen)

-- filepath
import System.OsPath (OsPath, encodeUtf, decodeUtf, (</>))
import System.FilePath ((<.>))

-- directory
import System.Directory.OsPath (createDirectoryIfMissing)

-- cassava
import Data.Csv
  (FromNamedRecord(..), ToNamedRecord(..), DefaultOrdered(..))

-- }}}

-- --< Benchmark / Config >-- {{{

-- | `Pristine.Benchmark` configuration.
data Config = MkConfig
  -- | How many \( \mu s \) to delay each run of the `Pristine.Benchmark`.
  --   Gives the OS opportunities to schedule work when it won't disturb our measurements.
  --
  --   Default: 10,000.
  { delay     :: !Int
  -- | How many samples to collect.
  --
  --   Default: 200.
  , samples   :: !Int
  -- | Directory to read\/write data from\/to.
  --
  --   Default: current.
  , directory :: FilePath
  }

-- }}}

-- --< Benchmark / Ensemble / Functions >-- {{{

-- | `NFData` and CSV constraints for arguments.
type Arg a = (NFData a, FromNamedRecord a, ToNamedRecord a, DefaultOrdered a)

-- | A type of random generators for @a@.
newtype Gen a = MkGen{ next :: StdGen -> (a, StdGen) }
 deriving (Functor, Applicative, Monad)
  via State StdGen

-- }}}

-- --< Internal: Paths >-- {{{

data Paths = MkPaths
  { timingFile :: {-# UNPACK #-} !(OsPath, FilePath)
  , statFile   :: {-# UNPACK #-} !(OsPath, FilePath)
  }

paths :: Config -> [Text] -> Text -> IO Paths
paths cnf parents name = do
  basedir  <- encodeUtf (directory cnf)
  parents' <- traverse (encodeUtf . T.unpack) parents
  let dir = basedir </> foldr (</>) mempty parents'
  createDirectoryIfMissing True dir

  timingCsv <- encodeUtf (T.unpack name <> " (timings)" <.> "csv")
  let timingOP = dir </> timingCsv
  timingFP  <- decodeUtf timingOP

  statCsv <- encodeUtf (T.unpack name <> " (rts stats)" <.> "csv")
  let statOP = dir </> statCsv
  statFP  <- decodeUtf statOP

  pure MkPaths
    { timingFile = (timingOP, timingFP)
    , statFile   = (statOP  , statFP  )
    }

-- }}}

-- --< Internal: RTS Stats Support >-- {{{

data SupportKind = Disabled | Enabled

data Support (s :: SupportKind) where
  IsDisabled :: Support Disabled
  IsEnabled  :: Support Enabled

deriving instance Show (Support s)

data UnknownSupport = forall s. Unknown !(Support s)

data Supported (s :: SupportKind) a where
  Unsupported ::      Supported Disabled a
  Supported   :: a -> Supported Enabled a

deriving instance Functor     (Supported s)
deriving instance Foldable    (Supported s)
deriving instance Traversable (Supported s)

checkRtsStatsSupport :: IO UnknownSupport
checkRtsStatsSupport = getRTSStatsEnabled <&> \enabled -> if enabled
  then Unknown IsEnabled
  else Unknown IsDisabled

-- }}}

-- --< Internal: CsvException >-- {{{

newtype CsvException = MkCsvException{ unMkCsvException :: String }

instance Show CsvException where
  show = unMkCsvException

instance Exception CsvException

csvErr :: String -> IO a
csvErr = throwIO . MkCsvException

-- }}}

