
<!-- README.md is generated from README.Rmd. Please edit that file. -->

# Price Index Aggregation in R

<!-- badges: start -->

[![R-CMD-check](https://github.com/marberts/piar/workflows/R-CMD-check/badge.svg)](https://github.com/marberts/piar/actions)
[![codecov](https://codecov.io/gh/marberts/piar/branch/master/graph/badge.svg?token=lHDHsGHsLd)](https://codecov.io/gh/marberts/piar)
<!-- badges: end -->

Most price indexes are made with a two-step procedure, where
period-over-period are first calculated for a collection of at each
point in time, and then aggregated according to a . These indexes can
then be chained together to form a time series that gives the evolution
of prices with respect to a fixed base period. This package contains a
collections of functions that revolve around this work flow, making it
easy to build standard price indexes, and implement the methods
described by Balk (2008, <ISBN:978-1-107-40496-0>), von der Lippe (2001,
<ISBN:3-8246-0638-0>), and the CPI manual (2020,
<ISBN:978-1-51354-298-0>) for bilateral price indexes.

## Installation

``` r
devtools::install_github(c("marberts/gpindex", "marberts/piar"))
```

## Usage

There is a detailed vignette showing how to use **piar**:
`browseVignettes("piar")`. But the basic work flow is fairly simple.

The starting point is to make period-over-period elemental price indexes
with the `elemental_index()` function and an aggregation structure with
the `aggregation_structure()` function. The `aggregate()` method can
then be used to aggregate the elemental indexes according to the
aggregation structure. There are a variety of methods to work with these
index objects, including chaining them over time.

``` r
library(piar)

# Make Jevons business-level elemental indexes

head(ms_prices)
#>   period business product price
#> 1 202001       B1       1  1.14
#> 2 202001       B1       2    NA
#> 3 202001       B1       3  6.09
#> 4 202001       B2       4  6.23
#> 5 202001       B2       5  8.61
#> 6 202001       B2       6  6.40

elementals <- with(
  ms_prices, 
  elemental_index(
    price_relative(price, period, product), 
    period, business, na.rm = TRUE
  )
)

# Make an aggregation structure from businesses to higher-level
# industrial classifications

head(ms_weights)
#>   business classification weight
#> 1       B1             11    553
#> 2       B2             11    646
#> 3       B3             11    312
#> 4       B4             12    622
#> 5       B5             12    330

pias <- with(
  ms_weights,
  aggregation_structure(
    c(expand_classification(classification), list(business)),
    weight
  )
)

# Aggregate elemental indexes with an arithmetic index

index <- aggregate(elementals, pias, na.rm = TRUE)

# Chain them to get a time series

cumprod(index)
#>    202001    202002    202003    202004
#> 1       1 1.3007239 1.3827662 3.7815355
#> 11      1 1.3007239 1.3827662 2.1771866
#> 12      1 1.3007239 1.3827662 6.3279338
#> B1      1 0.8949097 0.2991629 0.4710366
#> B2      1 1.3007239 1.3827662 3.8308934
#> B3      1 2.0200036 3.3033836 1.7772072
#> B4      1 1.3007239 1.3827662 6.3279338
#> B5      1 1.3007239 1.3827662 6.3279338
```
