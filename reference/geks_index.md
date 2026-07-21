# GEKS index

Calculate a generalized inter-temporal GEKS price index over a rolling
window.

## Usage

``` r
geks_index(
  price,
  quantity,
  period,
  product,
  index_formula = function(p1, p0, q1, q0) {
     nested_gmean(p1/p0, list(p0 * q0, p1 *
    q1), na.rm = TRUE)
 },
  window = nlevels(period),
  n = window - 1L,
  order = 0,
  match_method = c("all", "back-price")
)
```

## Arguments

- price:

  `[numeric > 0]` A numeric vector of prices, the same length as
  `quantity`.

- quantity:

  `[numeric >= 0]` A numeric vector of quantities, the same length as
  `price`.

- period:

  `[factor]` A factor, or something that can be coerced into one, that
  gives the corresponding time period for each element in `price` and
  `quantity`. The ordering of time periods follows the levels of
  `period` to agree with
  [`cut()`](https://rdrr.io/r/base/cut.POSIXt.html).

- product:

  `[factor]` A factor, or something that can be coerced into one, that
  gives the corresponding product identifier for each element in `price`
  and `quantity`.

- index_formula:

  `[function]` A function giving the index-number formula in the GEKS
  index. Usually a Törnqvist, Fisher (the default), or Walsh index. It
  must have arguments `p1`, `p0`, `q1`, and `q0`, and satisfy the
  time-reversal test. See
  [`vignette("index-number-formulas")`](https://marberts.github.io/piar/articles/index-number-formulas.md)
  for details.

- window:

  `[integer(1) > 0]` A positive integer giving the length of the rolling
  window. The default is a window that encompasses all periods in
  `period`. Non-integers are truncated towards zero.

- n:

  `[integer(1) > 0]` A positive integer giving the length of the index
  series for each window, starting from the end of the window. For
  example, if there are 13 periods in `window`, setting `n = 1` gives
  the index for period 13. The default gives an index for each period in
  `window`. Non-integers are truncated towards zero.

- order:

  `[numeric(1)]` A finite number giving the order of the generalized
  mean used to average price indexes over the rolling window. The
  default uses a geometric mean.

- match_method:

  `[character(1)]` Either `"all"` to match all products against each
  other (the default) or `"back-price"` to match only back prices. The
  later can be faster when there is lots of product imbalanced.

## Value

A list with a named numeric vector giving the value of the respective
period-over-period GEKS index for each window.

## Note

Like
[`back_period()`](https://marberts.github.io/piar/reference/back_period.md),
if multiple prices correspond to a period-product pair, then the back
price at a point in time is always the first price for that product in
the previous period. Unlike a bilateral index, however, duplicated
period-product pairs can have more subtle implications for a
multilateral index.

## References

Balk, B. M. (2008). *Price and Quantity Index Numbers*. Cambridge
University Press.

IMF, ILO, Eurostat, UNECE, OECD, and World Bank. (2020). *Consumer Price
Index Manual: Concepts and Methods*. International Monetary Fund.

Ivancic, L., Diewert, W. E., and Fox, K. J. (2011). Scanner data, time
aggregation and the construction of price indexes. *Journal of
Econometrics*, 161(1): 24–35.

## See also

`GEKSIndex()` in the IndexNumR package for an implementation of the GEKS
index with more options.

[`splice_index()`](https://marberts.github.io/piar/reference/splice_index.md)
to splice the rolling-window indexes together.

## Examples

``` r
price <- 1:10
quantity <- 10:1
period <- rep(1:5, 2)
product <- rep(letters[1:2], each = 5)

cumprod(geks_index(price, quantity, period, product)[[1]])
#>        2        3        4        5 
#> 1.407766 1.827832 2.274358 2.784143 

# Calculate the index over a rolling window.
(geks <- geks_index(price, quantity, period, product, window = 3))
#> [[1]]
#>        2        3 
#> 1.387429 1.292720 
#> 
#> [[2]]
#>        3        4 
#> 1.292347 1.238499 
#> 
#> [[3]]
#>        4        5 
#> 1.238857 1.206460 
#> 

# Use a movement splice to combine the indexes in each window.
splice_index(geks, 2)
#>        2        3        4        5 
#> 1.387429 1.793558 2.221320 2.679934 

# ... or use a mean splice.
splice_index(geks)
#>        2        3        4        5 
#> 1.387429 1.793558 2.221000 2.679934 

# Make a Jevons GEKS index.
geks_index(
  price,
  quantity,
  period,
  product,
  index_formula = \(p1, p0, ...) gmean(p1 / p0, na.rm = TRUE, order = 0)
)
#> [[1]]
#>        2        3        4        5 
#> 1.527525 1.309307 1.224745 1.178511 
#> 
```
