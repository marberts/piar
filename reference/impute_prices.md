# Impute missing prices

Impute missing prices using the carry forward or shadow price method.

## Usage

``` r
shadow_price(x, ...)

# Default S3 method
shadow_price(
  x,
  period,
  product,
  ea,
  ...,
  pias = NULL,
  weights = NULL,
  r1 = 0,
  r2 = 1
)

# S3 method for class 'data.frame'
shadow_price(x, formula, ..., weights = NULL)

carry_forward(x, ...)

# Default S3 method
carry_forward(x, period, product, ...)

# S3 method for class 'data.frame'
carry_forward(x, formula, ...)

carry_backward(x, ...)

# Default S3 method
carry_backward(x, period, product, ...)

# S3 method for class 'data.frame'
carry_backward(x, formula, ...)
```

## Arguments

- x:

  Either a numeric vector (or something that can be coerced into one) or
  data frame of prices.

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
  elementary aggregate associated with each price in `x`.

- pias:

  A price index aggregation structure, or something that can be coerced
  into one, as made with
  [`aggregation_structure()`](https://marberts.github.io/piar/reference/aggregation_structure.md).
  The default imputes from elementary indexes only (i.e., not
  recursively).

- weights:

  A numeric vector of weights for the prices in `x` (i.e., product
  weights), or something that can be coerced into one. The default is to
  give each price equal weight. This is evaluated in `x` for the data
  frame method.

- r1:

  Order of the generalized-mean price index used to calculate the
  elementary price indexes: 0 for a geometric index (the default), 1 for
  an arithmetic index, or -1 for a harmonic index. Other values are
  possible; see
  [`gpindex::generalized_mean()`](https://marberts.github.io/gpindex/reference/generalized_mean.html)
  for details.

- r2:

  Order of the generalized-mean price index used to aggregate the
  elementary price indexes: 0 for a geometric index, 1 for an arithmetic
  index (the default), or -1 for a harmonic index. Other values are
  possible; see
  [`gpindex::generalized_mean()`](https://marberts.github.io/gpindex/reference/generalized_mean.html)
  for details.

- formula:

  A two-sided formula with prices on the left-hand side. For
  `carry_forward()` and `carry_backward()`, the right-hand side should
  have time periods and products (in that order); for `shadow_price()`,
  the right-hand side should have time period, products, and elementary
  aggregates (in that order).

## Value

A numeric vector of prices with missing values replaced (where
possible).

## Details

The carry forward method replaces a missing price for a product by the
price for the same product in the previous period. It tends to push an
index value towards 1, and is usually avoided; see paragraph 6.61 in the
CPI manual (2020). The carry backwards method does the opposite, but
this is rarely used in practice.

The shadow price method recursively imputes a missing price by the value
of the price for the same product in the previous period multiplied by
the value of the period-over-period elementary index for the elementary
aggregate to which that product belongs. This requires computing and
aggregating an index (according to `pias`, unless `pias` is not
supplied) for each `period`, and so these imputations can take a while.
The index values used to do the imputations are not returned because the
index needs to be recalculated to get correct percent-change
contributions.

Shadow price imputation is referred to as self-correcting overall mean
imputation in chapter 6 of the CPI manual (2020). It is identical to
simply excluding missing price relatives in the index calculation,
except in the period that a missing product returns. For this reason
care is needed when using this method. It is sensitive to the assumption
that a product does not change over time, and in some cases it is safer
to simply omit the missing price relatives instead of imputing the
missing prices.

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

carry_forward(prices, price ~ period + product)
#> [1] 1 2 3 4 5 6 7 4

shadow_price(prices, price ~ period + product + ea)
#> [1]  1  2  3  4  5  6  7 12
```
