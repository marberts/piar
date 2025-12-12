# Multiple sources of data

Price indexes are usually made from several sources of data. An
important benefit of the usual two-step workflow to make price indexes
is that the elementary indexes can be built piecemeal—using different
sources of data and different index-number formulas—and then aggregated
with a consistent structure.

Let’s extend the example in
[`vignette("piar")`](https://marberts.github.io/piar/articles/piar.md)
by having an alternate source of data for business `B5` that is always
missing in the `ms_prices` dataset.

``` r
library(piar)

# Make an aggregation structure.
ms_weights[c("level1", "level2")] <-
  expand_classification(ms_weights$classification)

pias <- ms_weights[c("level1", "level2", "business", "weight")] |>
  as_aggregation_structure()

# Make elementary index.
elementals <- ms_prices |>
  transform(
    relative = price_relative(price, period = period, product = product)
  ) |>
  elementary_index(relative ~ period + business, na.rm = TRUE)

elementals
```

    ## Period-over-period price index for 4 levels over 4 time periods 
    ##       time
    ## levels 202001    202002    202003   202004
    ##     B1      1 0.8949097 0.3342939      NaN
    ##     B2      1       NaN       NaN 2.770456
    ##     B3      1 2.0200036 1.6353355 0.537996
    ##     B4    NaN       NaN       NaN 4.576286

Instead of using survey-like data for the other businesses, `B5` is made
from scanner-like data with many price and quantity observations at each
point in time.

``` r
set.seed(12345)

scanner_prices <- data.frame(
  period = rep(c("201904", time(elementals)), each = 200),
  product = 1:200,
  price = round(rlnorm(5 * 200) * 10, 1),
  quantity = round(runif(5 * 200, 100, 1000))
)

head(scanner_prices)
```

    ##   period product price quantity
    ## 1 201904       1  18.0      958
    ## 2 201904       2  20.3      660
    ## 3 201904       3   9.0      579
    ## 4 201904       4   6.4      903
    ## 5 201904       5  18.3      276
    ## 6 201904       6   1.6      896

These type of data often require the use of a multilateral index like
the GEKS. For the sake of illustration, we’ll make a Fisher GEKS index
over a 3 quarter rolling window and use a mean splice to make a single
time series.

``` r
library(gpindex)

geks_elementals <- with(
  scanner_prices,
  fisher_geks(price, quantity, period, product, window = 3)
) |>
  splice_index() |>
  t() |>
  as_index(chainable = FALSE) |>
  set_levels("B5") |>
  rebase("202001")

geks_elementals
```

    ## Fixed-base price index for 1 levels over 4 time periods 
    ##       time
    ## levels 202001   202002   202003    202004
    ##     B5      1 1.012081 1.143009 0.8109263

These values can now be merged with the other elementary indexes,
getting turned into a period-over-period index in the process, and then
aggregated.

``` r
merge(elementals, geks_elementals) |>
  aggregate(pias, na.rm = TRUE)
```

    ## Period-over-period price index for 8 levels over 4 time periods 
    ##       time
    ## levels 202001    202002    202003    202004
    ##     1       1 1.1891575 1.0848816 2.1434597
    ##     11      1 1.3007239 1.0630743 1.5745154
    ##     12      1 1.0120809 1.1293651 3.2358970
    ##     B1      1 0.8949097 0.3342939 1.5745154
    ##     B2      1 1.3007239 1.0630743 2.7704563
    ##     B3      1 2.0200036 1.6353355 0.5379960
    ##     B4      1 1.0120809 1.1293651 4.5762862
    ##     B5      1 1.0120809 1.1293651 0.7094664
