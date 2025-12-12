# Price Index Aggregation in R

Most price indexes are made with a two-step procedure, where
period-over-period *elementary indexes* are first calculated for a
collection of *elementary aggregates* at each point in time, and then
aggregated according to a *price index aggregation structure*. These
indexes can then be chained together to form a time series that gives
the evolution of prices with respect to a fixed base period. This
package contains a collection of functions that revolve around this work
flow, making it easy to build standard price indexes, and implement the
methods described by Balk (2008), von der Lippe (2007), and the CPI
manual (2020) / PPI manual (2004) for bilateral price indexes.

The tools in this package are designed to be useful for both researching
new sources of data and methods to construct price indexes, and the
regular production of price statistics. It is targeted towards
economists, statisticians, and data scientists working at national
statistical agencies, central banks, financial institutions, and in
academia that want to measure and study the evolution of prices over
time.

## Installation

Get the stable version from CRAN.

``` r
install.packages("piar")
```

The development version can be installed from R-Universe

``` r
install.packages(
  "piar",
  repos = c("https://marberts.r-universe.dev", "https://cloud.r-project.org")
)
```

or directly from Github.

``` r
pak::pak("marberts/piar")
```

## Usage

There are several detailed vignette showing how to use **piar**:
`browseVignettes("piar")`. But the basic work flow is fairly simple.

The starting point is to make period-over-period elementary price
indexes with the
[`elementary_index()`](https://marberts.github.io/piar/reference/elementary_index.md)
function.

``` r
library(piar)

# Make Jevons business-level elementary indexes

head(ms_prices)
#>   period business product price
#> 1 202001       B1       1  1.14
#> 2 202001       B1       2    NA
#> 3 202001       B1       3  6.09
#> 4 202001       B2       4  6.23
#> 5 202001       B2       5  8.61
#> 6 202001       B2       6  6.40

elementals <- ms_prices |>
  transform(
    relative = price_relative(price, period = period, product = product)
  ) |>
  elementary_index(relative ~ period + business, na.rm = TRUE)

elementals
#> Period-over-period price index for 4 levels over 4 time periods 
#>       time
#> levels 202001    202002    202003   202004
#>     B1      1 0.8949097 0.3342939      NaN
#>     B2      1       NaN       NaN 2.770456
#>     B3      1 2.0200036 1.6353355 0.537996
#>     B4    NaN       NaN       NaN 4.576286
```

And an aggregation structure.

``` r
# Make an aggregation structure from businesses to higher-level
# industrial classifications

ms_weights
#>   business classification weight level1 level2
#> 1       B1             11    553      1     11
#> 2       B2             11    646      1     11
#> 3       B3             11    312      1     11
#> 4       B4             12    622      1     12
#> 5       B5             12    330      1     12

ms_weights[c("level1", "level2")] <-
  expand_classification(ms_weights$classification)

pias <- ms_weights[c("level1", "level2", "business", "weight")]

pias
#>   level1 level2 business weight
#> 1      1     11       B1    553
#> 2      1     11       B2    646
#> 3      1     11       B3    312
#> 4      1     12       B4    622
#> 5      1     12       B5    330
```

The [`aggregate()`](https://rdrr.io/r/stats/aggregate.html) method can
then be used to aggregate the elementary indexes according to the
aggregation structure (the first three rows below) and fill in missing
elementary indexes while maintaining consistency in aggregation. There
are a variety of methods to work with these index objects, such as
chaining them over time.

``` r
# Aggregate elementary indexes with an arithmetic index

index <- aggregate(elementals, pias, na.rm = TRUE)

# Chain them to get a time series

chain(index)
#> Fixed-base price index for 8 levels over 4 time periods 
#>       time
#> levels 202001    202002    202003    202004
#>     1       1 1.3007239 1.3827662 3.7815355
#>     11      1 1.3007239 1.3827662 2.1771866
#>     12      1 1.3007239 1.3827662 6.3279338
#>     B1      1 0.8949097 0.2991629 0.4710366
#>     B2      1 1.3007239 1.3827662 3.8308934
#>     B3      1 2.0200036 3.3033836 1.7772072
#>     B4      1 1.3007239 1.3827662 6.3279338
#>     B5      1 1.3007239 1.3827662 6.3279338
```

## Contributing

All contributions are welcome. Please start by opening an issue on
GitHub to report any bugs or suggest improvements and new features. See
the contribution guidelines for this project for more information.

## References

Balk, B. M. (2008). *Price and Quantity Index Numbers*. Cambridge
University Press.

Chiru, R., Huang, N., Lequain, M. Smith, P., and Wright, A. (2015). *The
Canadian Consumer Price Index Reference Paper*, Statistics Canada
catalogue 62-553-X. Statistics Canada.

ILO, IMF, UNECE, OECD, and World Bank. (2004). *Producer Price Index
Manual: Theory and Practice*. International Monetary Fund.

IMF, ILO, Eurostat, UNECE, OECD, and World Bank. (2020). *Consumer Price
Index Manual: Concepts and Methods*. International Monetary Fund.

von der Lippe, P. (2007). *Index Theory and Price Statistics*. Peter
Lang.
