# piar: Price Index Aggregation in R

<!-- badges: start -->
[![R-CMD-check](https://github.com/marberts/piar/workflows/R-CMD-check/badge.svg)](https://github.com/marberts/piar/actions)
[![codecov](https://codecov.io/gh/marberts/piar/branch/master/graph/badge.svg?token=lHDHsGHsLd)](https://codecov.io/gh/marberts/piar)
<!-- badges: end -->

Most price indexes are made with a two-step procedure, where period-over-period *elemental indexes* are first calculated for a collection of *elemental aggregates* at each point in time, and then aggregated according to a *price index aggregation structure*. These indexes can then be chained together to form a time series that gives the evolution of prices with respect to a fixed base period. This package contains a collections of functions that revolve around this work flow, making it easy to build standard price indexes in **R** using the methods in Balk (2008, ISBN:978-1-107-40496-0) and ILO, IMF, OECD, Eurostat, UN, and World Bank (2020, ISBN:978-1-51354-298-0).

## Installation

**piar** requires version >= 0.3.3 of the **gpindex** package, which is ahead of the version on CRAN.

```r
devtools::install_github(c("marberts/gpindex", "marberts/piar"))
```

## Usage

There is a detailed vignette showing how to use **piar**: `browseVignettes("piar")`. But the basic work flow is fairly simple. 

The starting point is to make period-over-period elemental price indexes with the `elemental_index()` function and an aggregation structure with the `aggregation_structure()` function. The `aggregate()` method can then be used to aggregate the elemental indexes according to the aggregation structure. There are a variety of methods to work with these index objects, including chaining them over time.