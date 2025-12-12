# Calculate period-over-period price relatives

Construct period-over-period price relatives from information on prices
and products over time.

## Usage

``` r
price_relative(x, ...)

# Default S3 method
price_relative(x, ..., period, product)

# S3 method for class 'data.frame'
price_relative(x, formula, ...)
```

## Arguments

- x:

  Either a numeric vector (or something that can be coerced into one) or
  data frame of prices.

- ...:

  Further arguments passed to or used by methods.

- period:

  A factor, or something that can be coerced into one, that gives the
  corresponding time period for each element in `x`. The ordering of
  time periods follows the levels of `period` to agree with
  [`cut()`](https://rdrr.io/r/base/cut.POSIXt.html).

- product:

  A factor, or something that can be coerced into one, that gives the
  corresponding product identifier for each element in `x`.

- formula:

  A two-sided formula with prices on the left-hand side, and time
  periods and products (in that order) on the right-hand side.

## Value

A numeric vector of price relatives, with `product` as names.

## See also

[`gpindex::back_period()`](https://marberts.github.io/gpindex/reference/back_period.html)
to get only the back price.

[`gpindex::base_period()`](https://marberts.github.io/gpindex/reference/back_period.html)
for making fixed-base price relatives.

[`carry_forward()`](https://marberts.github.io/piar/reference/impute_prices.md)
and
[`shadow_price()`](https://marberts.github.io/piar/reference/impute_prices.md)
to impute missing prices.

[`gpindex::outliers`](https://marberts.github.io/gpindex/reference/outliers.html)
for methods to identify outliers with price relatives.

## Examples

``` r
price_relative(
  1:6,
  period = rep(1:2, each = 3),
  product = rep(letters[1:3], 2)
)
#>   a   b   c   a   b   c 
#> 1.0 1.0 1.0 4.0 2.5 2.0 
```
