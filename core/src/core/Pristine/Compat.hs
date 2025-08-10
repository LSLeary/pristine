
{- |

Description : Limited compatibility shims for /criterion/ & co.
Copyright   : (c) 2026 L. S. Leary

Limited compatibility shims for /criterion/ & co.

-}
module Pristine.Compat (

  bench,
  bgroup,

  perRunEnv,
  perRunEnvWithCleanup,

  nf, whnf,
  nfIO, whnfIO,
  nfAppIO, whnfAppIO,

) where

-- base
import Data.Foldable (fold)

-- deepseq
import Control.DeepSeq (NFData)

-- text
import Data.Text (Text)

-- pristine
import Pristine.Measurement
  ( Ensemble, (=:=), section, Benchmarkable
  , io, io', ($$), (!$), ($$$), (!$$)
  , env', ultimately
  )

-- | @bench = (=:=)@
bench :: Text -> p -> Ensemble c p
bench = (=:=)

-- | @bgroup name = `section` name . `fold`@
bgroup :: Foldable t => Text -> t (Ensemble c p) -> Ensemble c p
bgroup name = section name . fold

-- | @perRunEnv getEnv withEnv = `io'` . withEnv `<$>` `env'` getEnv@
perRunEnv
  :: (NFData env, NFData b)
  => IO env -> (env -> IO b) -> Benchmarkable
perRunEnv getEnv withEnv = io' . withEnv <$> env' getEnv

{- |
@
perRunEnvWithCleanup getEnv cleanup withEnv = do
  e <- `env'` getEnv
  `ultimately` (cleanup e)
  pure $ `io'` (withEnv e)
@
-}
perRunEnvWithCleanup
  :: (NFData env, NFData b)
  => IO env -> (env -> IO ()) -> (env -> IO b) -> Benchmarkable
perRunEnvWithCleanup getEnv cleanup withEnv = do
  e <- env' getEnv
  ultimately (cleanup e)
  pure $ io' (withEnv e)

-- | @nf f x = `pure` (f `!$` x)@
nf :: NFData b => (a -> b) -> a -> Benchmarkable
nf f x = pure (f !$ x)

-- | @whnf f x = `pure` (f `$$` x)@
whnf :: (a -> b) -> a -> Benchmarkable
whnf f x = pure (f $$ x)

-- | @nfIO = `pure` `.` `io'`@
nfIO :: NFData a => IO a -> Benchmarkable
nfIO = pure . io'

-- | @whnfIO = `pure` `.` `io`@
whnfIO :: IO a -> Benchmarkable
whnfIO = pure . io

-- | @nfAppIO p x = `pure` (p `!$$` x)@
nfAppIO :: NFData b => (a -> IO b) -> a -> Benchmarkable
nfAppIO p x = pure (p !$$ x)

-- | @whnfAppIO p x = `pure` (p `$$$` x)@
whnfAppIO :: (a -> IO b) -> a -> Benchmarkable
whnfAppIO p x = pure (p $$$ x)

