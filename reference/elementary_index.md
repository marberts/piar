# Make elementary/elemental price indexes

Compute period-over-period (chainable) or fixed-base (direct) elementary
price indexes, with optional percent-change contributions for each
product.

## Usage

``` r
elementary_index(x, ...)

# Default S3 method
elementary_index(x, ...)

# S3 method for class 'numeric'
elementary_index(
  x,
  ...,
  period = gl(1, length(x)),
  ea = gl(1, length(x)),
  weights = NULL,
  product = NULL,
  chainable = TRUE,
  na.rm = FALSE,
  contrib = FALSE,
  r = 0
)

# S3 method for class 'data.frame'
elementary_index(x, formula, ..., weights = NULL, product = NULL)

elemental_index(x, ...)
```

## Arguments

- x:

  Period-over-period or fixed-base price relatives. Currently there are
  methods for numeric vectors (which can be made with
  [`price_relative()`](https://marberts.github.io/piar/reference/price_relative.md))
  and data frames.

- ...:

  Further arguments passed to or used by methods.

- period:

  A factor, or something that can be coerced into one, giving the time
  period associated with each price relative in `x`. The ordering of
  time periods follows of the levels of `period`, to agree with
  [`cut()`](https://rdrr.io/r/base/cut.POSIXt.html). The default makes
  an index for one time period.

- ea:

  A factor, or something that can be coerced into one, giving the
  elementary aggregate associated with each price relative in `x`. The
  default makes an index for one elementary aggregate.

- weights:

  A numeric vector of weights for the price relatives in `x`, or
  something that can be coerced into one. The default is equal weights.
  This is evaluated in `x` for the data frame method.

- product:

  A character vector of product names, or something that can be coerced
  into one, for each price relative in `x` when making percent-change
  contributions. The default uses the names of `x`, if any; otherwise,
  elements of `x` are given sequential names within each elementary
  aggregate. This is evaluated in `x` for the data frame method.

- chainable:

  Are the price relatives in `x` period-over-period relatives that are
  suitable for a chained calculation (the default)? This should be
  `FALSE` when `x` contains fixed-base relatives.

- na.rm:

  Should missing values be removed? By default, missing values are not
  removed. Setting `na.rm = TRUE` is equivalent to overall mean
  imputation.

- contrib:

  Should percent-change contributions be calculated? The default does
  not calculate contributions.

- r:

  Order of the generalized mean to aggregate price relatives. 0 for a
  geometric index (the default for making elementary indexes), 1 for an
  arithmetic index (the default for aggregating elementary indexes and
  averaging indexes over subperiods), or -1 for a harmonic index
  (usually for a Paasche index). Other values are possible; see
  [`gpindex::generalized_mean()`](https://marberts.github.io/gpindex/reference/generalized_mean.html)
  for details.

- formula:

  A two-sided formula with price relatives on the left-hand side, and
  time periods and elementary aggregates (in that order) on the
  right-hand side.

## Value

A price index that inherits from
[`piar_index`](https://marberts.github.io/piar/reference/piar_index.md).
If `chainable = TRUE` then this is a period-over-period index that also
inherits from
[`chainable_piar_index`](https://marberts.github.io/piar/reference/piar_index.md);
otherwise, it is a fixed-based index that inherits from
[`direct_piar_index`](https://marberts.github.io/piar/reference/piar_index.md).

## Details

When supplied with a numeric vector, `elementary_index()` is a simple
wrapper that applies
[`gpindex::generalized_mean(r)()`](https://marberts.github.io/gpindex/reference/generalized_mean.html)
and
[`gpindex::contributions(r)()`](https://marberts.github.io/gpindex/reference/contributions.html)
(if `contrib = TRUE`) to `x` and `weights` grouped by `ea` and `period`.
That is, for every combination of elementary aggregate and time period,
`elementary_index()` calculates an index based on a generalized mean of
order `r` and, optionally, percent-change contributions. Product names
should be unique within each time period when making contributions, and,
if not, are passed to
[`make.unique()`](https://rdrr.io/r/base/make.unique.html) with a
warning. The default (`r = 0` and no weights) makes Jevons elementary
indexes. See chapter 8 (pp. 175–190) of the CPI manual (2020) for more
detail about making elementary indexes, or chapter 9 of the PPI manual
(2004), and chapter 5 of Balk (2008).

The default method simply coerces `x` to a numeric vector prior to
calling the method above. The data frame method provides a formula
interface to specify columns of price relatives, time periods, and
elementary aggregates and call the method above.

The interpretation of the index depends on how the price relatives in
`x` are made. If these are period-over-period relatives, then the result
is a collection of period-over-period (chainable) elementary indexes; if
these are fixed-base relatives, then the result is a collection of
fixed-base (direct) elementary indexes. For the latter, `chainable`
should be set to `FALSE` so that no subsequent methods assume that a
chained calculation should be used.

By default, missing price relatives in `x` will propagate throughout the
index calculation. Ignoring missing values with `na.rm = TRUE` is the
same as overall mean (parental) imputation, and needs to be explicitly
set in the call to `elementary_index()`. Explicit imputation of missing
relatives, and especially imputation of missing prices, should be done
prior to calling `elementary_index()`.

Indexes based on nested generalized means, like the Fisher index (and
superlative quadratic mean indexes more generally), can be calculated by
supplying the appropriate weights with
[`gpindex::nested_transmute()`](https://marberts.github.io/gpindex/reference/transmute_weights.html);
see the example below. It is important to note that there are several
ways to make these weights, and this affects how percent-change
contributions are calculated.

`elemental_index()` is an alias for `elementary_index()`.

## References

Balk, B. M. (2008). *Price and Quantity Index Numbers*. Cambridge
University Press.

ILO, IMF, UNECE, OECD, and World Bank. (2004). *Producer Price Index
Manual: Theory and Practice*. International Monetary Fund.

IMF, ILO, OECD, Eurostat, UNECE, and World Bank. (2020). *Consumer Price
Index Manual: Concepts and Methods*. International Monetary Fund.

von der Lippe, P. (2007). *Index Theory and Price Statistics*. Peter
Lang.

## See also

[`price_relative()`](https://marberts.github.io/piar/reference/price_relative.md)
for making price relatives for the same products over time, and
[`carry_forward()`](https://marberts.github.io/piar/reference/impute_prices.md)
and
[`shadow_price()`](https://marberts.github.io/piar/reference/impute_prices.md)
for imputation of missing prices.

[`as_index()`](https://marberts.github.io/piar/reference/as_index.md) to
turn pre-computed (elementary) index values into an index object.

[`chain()`](https://marberts.github.io/piar/reference/chain.md) for
chaining period-over-period indexes, and
[`rebase()`](https://marberts.github.io/piar/reference/chain.md) for
rebasing an index.

[`aggregate()`](https://marberts.github.io/piar/reference/aggregate.piar_index.md)
to aggregate elementary indexes according to an aggregation structure.

[`as.matrix()`](https://marberts.github.io/piar/reference/as.data.frame.piar_index.md)
and
[`as.data.frame()`](https://marberts.github.io/piar/reference/as.data.frame.piar_index.md)
for coercing an index into a tabular form.

## Examples

``` r
library(gpindex)

prices <- data.frame(
  rel = 1:8,
  period = rep(1:2, each = 4),
  ea = rep(letters[1:2], 4)
)

# Calculate Jevons elementary indexes

elementary_index(prices, rel ~ period + ea)
#> Period-over-period price index for 2 levels over 2 time periods 
#>       time
#> levels        1        2
#>      a 1.732051 5.916080
#>      b 2.828427 6.928203

# Same as using lm() or tapply()

exp(coef(lm(log(rel) ~ ea:factor(period) - 1, prices)))
#> eaa:factor(period)1 eab:factor(period)1 eaa:factor(period)2 eab:factor(period)2 
#>            1.732051            2.828427            5.916080            6.928203 

with(
  prices,
  t(tapply(rel, list(period, ea), geometric_mean, na.rm = TRUE))
)
#>          1        2
#> a 1.732051 5.916080
#> b 2.828427 6.928203

# A general function to calculate weights to turn the geometric
# mean of the arithmetic and harmonic mean (i.e., Fisher mean)
# into an arithmetic mean

fw <- grouped(nested_transmute(0, c(1, -1), 1))

# Calculate a CSWD index (same as the Jevons in this example)
# as an arithmetic index by using the appropriate weights

elementary_index(
  prices,
  rel ~ period + ea,
  weights = fw(rel, group = interaction(period, ea)),
  r = 1
)
#> Period-over-period price index for 2 levels over 2 time periods 
#>       time
#> levels        1        2
#>      a 1.732051 5.916080
#>      b 2.828427 6.928203
```
