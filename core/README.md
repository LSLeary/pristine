# *pristine*

A suite of benchmarking libraries with an emphasis on collecting, visualising and analysing pristine data.

## Key Differences from Prior Art

Comparisons with *criterion* and *tasty-bench*.
*gauge* is excluded on grounds of deprecation.

TL;DR: *pristine*

 * Treasures your data.
 * Supports benchmarking functions across a range of arguments.
 * Doesn't force batching on you.
 * Offers more suitable, less fragile analyses.
 * Evens out interference.
 * Has more powerful, elegant and hygienic resource management.
 * Is more composable.

### Data Management

*criterion* & co. are quite eager to discard their measurements, though they can presumably be coaxed into keeping and reusing them *somehow*.

In comparison, *pristine* treats its eponymously pristine data as a precious resource.
It never holds a substantial amount in volatile memory, instead streaming it to disk.
Even if the process is interrupted, it will flush buffers given the chance.

Having pre-existing data on disk is an expected operating condition, so it's never overwritten.
Instead, it's tallied up to check if the requested sample size is already accounted for, or if the data needs to be extended.

This means you can change your mind about what sample size, analysis or visualisation you want, re-run, and get the results quickly without redoing any measurements.

The tradeoff is that this approach is inherently stateful, and **care needs to be taken with that state to avoid bogus results**:

 * Benchmarks should not change between runs, only their configuration.
 * Benchmarks should be compiled with `-fproc-alignment=64`.
 * Data should be gathered under reasonably similar conditions.
 * Data should not be shared between different machines.

The CSV files should be manually deleted (or moved out of the way) when the above do not hold.

### Functional Benchmarks

*criterion* & co. measure fixed actions with no notion of dependency beyond resources.
However, in practice, you often want to see how the performance of some function varies with a parameter, or even compare the performance of multiple functions against the same parameter.
Doing so with *criterion* & co. is extremely clunky and coarse.

*pristine*, on the other hand, supports benchmarking functions natively by taking generators for their arguments.

### Batching

Unlike *pristine*, *criterion* & co. always *batch*.
This means that⁠—⁠instead of measuring precisely what you asked them to⁠—⁠they run your benchmark n times and measure that instead.
The result is non-pristine data suitable for calculating the mean, but not for arbitrary analysis.

Batching also means hogging the CPU for longer, increasing the likelihood that the OS intervenes during measurement and silently harms the entire batch.
*pristine*, on the other hand, intentionally leaves gaps between measurements for the OS to utilise.
When a measurement is interrupted regardless, the delay is concentrated into a single data point, allowing it to be identified as an outlier and removed.

Since batching is a special case of functional benchmarking, *pristine* supports it nevertheless:

```haskell
batch :: Text -> Benchmarkable -> Benchmark
batch name bm
  = argument @("n" ::: Int) (uniform (1, 1000))
  $ name =:= \(Row n) -> n `stimes` bm
```

### Robust Analysis

*criterion* & co. are quite fixated on *mean* and *standard deviation* as their measures of central tendency and dispersion.
That's fine in theory, but in practice benchmarking data is often skewed by asymmetric noise, rendering them unsuitable.

The robust alternatives *pristine-analysis* uses are the median and your choice of inter-quantile ranges.

Without making any assumptions about underlying distributions, quantile regression is able to fit polynomials to these quantiles, enabling both interpolation and extrapolation.
Polynomial degree may be specified precisely or selected by penalised cross validation.

### Interference

It's a relatively well known problem with benchmarking in Haskell that one benchmark can interfere with another.

*pristine* cannot fundamentally eliminate this interference, but it can *whiten* it into noise by destroying correlations.
In practice, this means interleaving benchmarks and selecting arguments randomly.

Running a control benchmark alongside the others allows the effect of interference to be estimated, and in principle, corrected for.

### Resource/Environment Management

*criterion* has a small zoo of specialised combinators for handling the environment:

```haskell
env :: NFData env => IO env -> (env -> Benchmark) -> Benchmark
envWithCleanup :: NFData env => IO env -> (env -> IO a) -> (env -> Benchmark) -> Benchmark
perBatchEnv :: (NFData env, NFData b) => (Int64 -> IO env) -> (env -> IO b) -> Benchmarkable
perBatchEnvWithCleanup :: (NFData env, NFData b) => (Int64 -> IO env) -> (Int64 -> env -> IO ()) -> (env -> IO b) -> Benchmarkable
perRunEnv :: (NFData env, NFData b) => IO env -> (env -> IO b) -> Benchmarkable
perRunEnvWithCleanup :: (NFData env, NFData b) => IO env -> (env -> IO ()) -> (env -> IO b) -> Benchmarkable
```

Despite all of this, there's no support for *scoped* resources such as *ki*'s `Scope`, which is only valid within the supplied continuation:

```haskell
scoped :: (Scope -> IO a) -> IO a
```

*tasty-bench* is comparatively impoverished, supporting only `env` and `envWithCleanup` (*tasty*'s `withResource` being essentially equivalent to the latter).

In *pristine* the situation is very different; we introduce a `Resource` monad that supports scoped resources directly:

```haskell
res_ :: (forall x. (a -> IO x) -> IO x) -> Resource a
```

`res_` is the complete core interface to creating `Resource` values, supporting everything in the zoo and more, but various conveniences like `env :: NFData env => IO env -> Resource env` are written on top of it.

`Benchmarkable` is just a synonym of `Resource Action`, so per-run resources are right at hand, while `Benchmark`⁠—⁠a synonym of `Ensemble Config Benchmarkable`⁠—⁠has

```haskell
with :: Resource r -> Ensemble c (r -> p) -> Ensemble c p
```

providing resources to enclosed benchmarks.

Note also that, while the `env*` operations of *criterion* & co. are unhygienically `Monad`ic, `with` is distinctly `Applicative`.
This design is carefully chosen to preserve the introspectability of `Ensemble`, which you can see in *pristine-example* is quite convenient for the analysis and plotting that tends to follow.

### Composability

In *criterion* & co., `Benchmarkable` has no useful combinators for composition; if you want to combine them you have to import their constructors and look under the hood.
`Benchmark`, on the other hand, only has combinators that assume you're making a *named group*.

In *pristine*, both have `Monoid`, as does the underlying `Action` type.

## The Suite

To prevent incurring unwanted dependencies and allow alternative analysis and visualisation components, *pristine* is not a monolith, but a suite of component libraries.

### *pristine*

The light-weight core.

Takes pristine measurements, streaming them to disk in a simple CSV format so you can later visualise or analyse the data any way you please.

### *pristine-analysis*

Incurs a dependency on *hmatrix*.

Provides simple statistical analysis for single-argument benchmarks which is robust to outliers and makes no assumptions about underlying distributions.

### *pristine-vis-Chart*

Incurs a dependency on *Chart*.

Makes scatter plots from single-argument benchmarks, optionally plotting estimated medians and your choice of inter-quantile ranges.

### *pristine-example*

An annotated example benchmark showing how all of the above can be used in conjunction⁠—⁠[check it out!](/example/src/Pristine/Example.hs)

![example output](/example/example.svg)

