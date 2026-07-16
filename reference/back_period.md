# Back period

Offset a vector prices or quantities by computing the position one (or
more) period backwards for each product.

## Usage

``` r
back_period(period, product = NULL, match_first = TRUE, offset = 1L)
```

## Arguments

- period:

  `[factor]` A factor, or something that can be coerced into one, that
  gives the time period for each transaction. The ordering of time
  periods follows the levels of `period` to agree with
  [`cut()`](https://rdrr.io/r/base/cut.POSIXt.html).

- product:

  `[factor]` A factor, or something that can be coerced into one, that
  gives the product identifier for each transaction. The default is to
  assume that all transactions are for the same product.

- match_first:

  `[logical(1)]` Should products in the first period match with
  themselves (the default)?

- offset:

  `[integer(1)]` The number of periods to offset. The default offsets by
  one period (back period). Setting to `nlevels(period)` gives the the
  base period.

## Value

A numeric vector of indices giving the position of the the back periods.

## Note

By definition, there must be at most one transaction for each product in
each time period to determine a back period. If multiple transactions
correspond to a period-product pair, then the back period at a point in
time is always the first position for that product in the previous
period.

## See also

[`outliers()`](https://marberts.github.io/piar/reference/outliers.md)
for common methods to detect outliers for price relatives.

`rs_pairs` in the rsmatrix package for making sales pairs.

## Examples

``` r
prices <- data.frame(
  price = 1:6,
  product = factor(c("a", "b")),
  period = factor(c(1, 1, 2, 2, 3, 3))
)

with(prices, back_period(period, product))
#> [1] 1 2 1 2 3 4

# Make fixed-base price relatives.
with(
  prices,
  price / price[back_period(period, product, offset = nlevels(period))]
)
#> [1] 1 1 3 2 5 3

# Change the base period with relevel().
with(
  prices,
  price / price[
    back_period(relevel(period, "2"), product, offset = nlevels(period))
  ]
)
#> [1] 0.3333333 0.5000000 1.0000000 1.0000000 1.6666667 1.5000000
```
