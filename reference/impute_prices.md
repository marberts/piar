# Impute missing prices

Impute missing prices using the carry forward/backward or the
self-correcting overall mean method.

## Usage

``` r
impute_prices(x, ...)

# Default S3 method
impute_prices(x, ...)

# S3 method for class 'matrix'
impute_prices(
  x,
  period,
  product,
  ...,
  ea = NULL,
  weights = NULL,
  pias = NULL,
  r = c(0, 1),
  method = c("overall-mean", "carry-forward")
)

# S3 method for class 'numeric'
impute_prices(
  x,
  period,
  product,
  ...,
  ea = NULL,
  weights = NULL,
  pias = NULL,
  r = c(0, 1),
  method = c("overall-mean", "carry-forward", "carry-backward")
)

# S3 method for class 'data.frame'
impute_prices(x, formula, ..., ea = NULL, weights = NULL)

carry_forward(x, ...)

carry_backward(x, ...)

shadow_price(x, ...)
```

## Arguments

- x:

  Either a numeric vector (or something that can be coerced into one), a
  data frame of prices, or a two-column matrix of current prices and
  back prices (in that order).

- ...:

  Further arguments passed to or used by methods.

- period:

  A factor, or something that can be coerced into one, giving the time
  period associated with each price in `x`. The ordering of time periods
  follows of the levels of `period`, to agree with
  [`cut()`](https://rdrr.io/r/base/cut.POSIXt.html).

- product:

  A factor, or something that can be coerced into one, giving the
  product associated with each price in `x`.

- ea:

  A factor, or something that can be coerced into one, giving the
  elementary aggregate associated with each price in `x`. This is
  evaluated in `x` for the data frame method. The default pools all data
  into one elementary aggregate.

- weights:

  A numeric vector of weights for the prices in `x` (i.e., product
  weights), or something that can be coerced into one. The default is to
  give each price equal weight. This is evaluated in `x` for the data
  frame method.

- pias:

  A price index aggregation structure, or something that can be coerced
  into one, as made with
  [`aggregation_structure()`](https://marberts.github.io/piar/reference/aggregation_structure.md).
  The default imputes from elementary indexes only (i.e., not
  recursively).

- r:

  A pair of numeric values. The first gives the order of the
  generalized-mean price index used to calculate the elementary price
  indexes, defaulting to a geometric index. The second gives the order
  of the generalized-mean price index used to aggregate the elementary
  price indexes, defaulting to an arithmetic index. Other values are
  possible; see
  [`gpindex::generalized_mean()`](https://marberts.github.io/gpindex/reference/generalized_mean.html)
  for details.

- method:

  Name of the imputation method, one of `"overall-mean"`,
  `"carry-forward"`, or `"carry-backward"`.

- formula:

  A two-sided formula, or something that can be coerced into one, with
  prices on the left-hand side and time periods and products on the
  right-hand side (in that order).

## Value

A numeric vector or matrix of prices with missing values replaced (where
possible).

## Details

The carry forward method replaces a missing price for a product by the
price for the same product in the previous period. It tends to push an
index value towards 1, and is usually avoided; see paragraph 6.61 in the
CPI manual (2020). The carry backwards method does the opposite, but
this is rarely used in practice.

The self-correcting overall mean method recursively imputes a missing
price by the value of the price for the same product in the previous
period multiplied by the value of the period-over-period elementary
index for the elementary aggregate to which that product belongs. This
requires computing and aggregating an index (according to `pias`, unless
`pias` is not supplied) for each `period`. The index values used to do
the imputations are not returned because the index needs to be
recalculated to get correct percent-change contributions. It is
identical to simply excluding missing price relatives in the index
calculation, except in the period that a missing product returns. For
this reason care is needed when using this method. It is sensitive to
the assumption that a product does not change over time, and in some
cases it is safer to simply omit the missing price relatives instead of
imputing the missing prices.

Imputation works slightly differently depending on whether data are in a
long or wide format. When `x` is a two-column of matrix of current and
back prices (in that order), then imputation is done separately on the
current price at a point in time and the back price at the next point in
time. When `x` is a numeric vector then these two prices are necessarily
the same.

## References

IMF, ILO, OECD, Eurostat, UNECE, and World Bank. (2020). *Consumer Price
Index Manual: Concepts and Methods*. International Monetary Fund.

## See also

[`price_relative()`](https://marberts.github.io/piar/reference/price_relative.md)
for making price relatives for the same products over time.

## Examples

``` r
prices <- data.frame(
  price = c(1:7, NA),
  period = rep(1:2, each = 4),
  product = 1:4,
  ea = rep(letters[1:2], 4)
)

impute_prices(prices, price ~ period + product, method = "carry-forward")
#> [1] 1 2 3 4 5 6 7 4

impute_prices(
  prices,
  price ~ period + product,
  ea = ea,
  method = "overall-mean"
)
#> [1]  1  2  3  4  5  6  7 12

# Can also be done with current price-back price formulation.
prices$back_price <- with(
  prices,
  price[gpindex::back_period(period, product)]
)

impute_prices(
  prices,
  cbind(price, back_price) ~ period + product,
  ea = ea,
  method = "overall-mean"
)
#>      price back_price
#> [1,]     1          1
#> [2,]     2          2
#> [3,]     3          3
#> [4,]     4          4
#> [5,]     5          1
#> [6,]     6          2
#> [7,]     7          3
#> [8,]    12          4
```
