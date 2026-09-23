# statideas

**Statistics Ideas Through Interactive Apps**

`statideas` demonstrates statistical ideas and concepts through interactive Shiny applications. Each
app is a self-contained teaching tool covering one topic — Bayesian trial design, group sequential
boundaries, outcome ranking, multiplicity control, and minimum detectable difference.

## Installation

```r
remotes::install_github("olssol/statideas")
```

The package compiles Stan models at install time, so a working C++ toolchain is required. On macOS
you will also need `cmake` (a build-time requirement of `RcppParallel`, which is not bundled with
Xcode or its Command Line Tools):

```
brew install cmake
```

Expect the install to take several minutes — three Stan models are compiled from source.

## Usage

Launch any app with `si_shiny()`:

```r
library(statideas)

si_shiny("boundary")     # group sequential design boundaries
si_shiny("bayesian")     # introduction to Bayesian clinical trial design
si_shiny("bayesian2")    # Bayesian concepts and Bayesian designs
si_shiny("door")         # Desirability of Outcome Ranking (DOOR)
si_shiny("mdd")          # understanding minimum detectable difference
si_shiny("mtp")          # multiplicity control
```

The Stan models can also be called directly:

```r
fit <- si_stan(
    list(ns = 3L, y = c(5L, 7L, 9L), n = c(20L, 20L, 20L), pri_sig = 2),
    stan_mdl = "hier"
)
```

Available models are `hier` (hierarchical binomial), `mixture`, and `logn`.

## Optional dependency

The `mtp` app additionally requires [`optMTP`](https://github.com/olssol/optMTP), which is not on
CRAN and pulls in `gMCP` → `rJava` → a Java JDK:

```r
remotes::install_github("olssol/optMTP")
```

Every other app works without it.

## Authors

Chenguang Wang and Jeen Liu — a Statistical Innovation project.

## License

GPL (>= 3). See [LICENSE.md](LICENSE.md).
