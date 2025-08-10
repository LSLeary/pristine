
{- |

Description : Rendering charts with /cairo/
Copyright   : (c) 2026 L. S. Leary

Rendering charts with /cairo/.

-}
module Pristine.Vis.Chart.Cairo (

  -- * Render
  render,

  -- * Re-exports
  PlotValue,
  FileFormat(..),
  Layout,

) where

-- base
import Data.Functor (void)

-- filepath
import System.OsPath (encodeUtf, takeDirectory)

-- directory
import System.Directory.OsPath (createDirectoryIfMissing)

-- Chart
import Graphics.Rendering.Chart.Axis.Types (PlotValue)
import Graphics.Rendering.Chart.Layout (Layout)
import Graphics.Rendering.Chart.Renderable (toRenderable)

-- Chart-cairo
import Graphics.Rendering.Chart.Backend.Cairo
  (renderableToFile, FileOptions(..), FileFormat(..))

-- | Render a plot to file.
render
  :: (PlotValue x, PlotValue y)
  => FileFormat
  -> FilePath
  -> Int -- ^ Width
  -> Int -- ^ Height
  -> Layout x y
  -> IO ()
render fmt filepath w h layout = do
  ospath <- encodeUtf filepath
  let dir = takeDirectory ospath
  createDirectoryIfMissing True dir
  void $ renderableToFile fopts filepath (toRenderable layout)
 where
  fopts = FileOptions
    { _fo_size   = (w, h)
    , _fo_format = fmt
    }

