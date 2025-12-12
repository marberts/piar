# Aggregate a price index over subperiods

Aggregate an index over subperiods by taking the (usually arithmetic)
mean of index values over consecutive windows of subperiods.

## Usage

``` r
# S3 method for class 'chainable_piar_index'
mean(
  x,
  ...,
  weights = NULL,
  window = ntime(x),
  na.rm = FALSE,
  contrib = TRUE,
  r = 1,
  duplicate_contrib = c("make.unique", "sum")
)

# S3 method for class 'direct_piar_index'
mean(
  x,
  ...,
  weights = NULL,
  window = ntime(x),
  na.rm = FALSE,
  contrib = TRUE,
  r = 1,
  duplicate_contrib = c("make.unique", "sum")
)
```

## Arguments

- x:

  A price index, as made by, e.g.,
  [`elementary_index()`](https://marberts.github.io/piar/reference/elementary_index.md).

- ...:

  Not currently used.

- weights:

  A numeric vector of weights for the index values in `x`, or something
  that can be coerced into one. The default is equal weights. It is
  usually easiest to specify these weights as a matrix with a row for
  each index value in `x` and a column for each time period.

- window:

  A positive integer giving the size of the window used to average index
  values across subperiods. The default averages over all periods in
  `x`. Non-integers are truncated towards 0.

- na.rm:

  Should missing values be removed? By default, missing values are not
  removed. Setting `na.rm = TRUE` is equivalent to overall mean
  imputation.

- contrib:

  Aggregate percent-change contributions in `x` (if any)?

- r:

  Order of the generalized mean to aggregate index values. 0 for a
  geometric index (the default for making elementary indexes), 1 for an
  arithmetic index (the default for aggregating elementary indexes and
  averaging indexes over subperiods), or -1 for a harmonic index
  (usually for a Paasche index). Other values are possible; see
  [`gpindex::generalized_mean()`](https://marberts.github.io/gpindex/reference/generalized_mean.html)
  for details.

- duplicate_contrib:

  The method to deal with duplicate product contributions. Either
  'make.unique' to make duplicate product names unique with
  [`make.unique()`](https://rdrr.io/r/base/make.unique.html) or 'sum' to
  add contributions for the same products across subperiods.

## Value

A price index, averaged over subperiods, that inherits from the same
class as `x`.

## Details

The [`mean()`](https://rdrr.io/r/base/mean.html) method constructs a set
of non-overlapping windows of length `window`, starting in the first
period of the index, and takes the mean of each index value in these
windows for each level of the index. The last window is discarded if it
is incomplete (with a warning), so that index values are always averaged
over `window` periods. The names for the first time period in each
window form the new names for the aggregated time periods.

Percent-change contributions are aggregated if `contrib = TRUE`
following the same approach as
[`aggregate()`](https://marberts.github.io/piar/reference/aggregate.piar_index.md).

An optional vector of weights can be specified when aggregating index
values over subperiods, which is often useful when aggregating a Paasche
index; see section 4.3 of Balk (2008) for details.

## References

Balk, B. M. (2008). *Price and Quantity Index Numbers*. Cambridge
University Press.

## See also

Other index methods:
[`[.piar_index()`](https://marberts.github.io/piar/reference/sub-.piar_index.md),
[`aggregate.piar_index`](https://marberts.github.io/piar/reference/aggregate.piar_index.md),
[`as.data.frame.piar_index()`](https://marberts.github.io/piar/reference/as.data.frame.piar_index.md),
[`as.ts.piar_index()`](https://marberts.github.io/piar/reference/as.ts.piar_index.md),
[`chain()`](https://marberts.github.io/piar/reference/chain.md),
[`contrib()`](https://marberts.github.io/piar/reference/contrib.md),
[`head.piar_index()`](https://marberts.github.io/piar/reference/head.piar_index.md),
[`is.na.piar_index()`](https://marberts.github.io/piar/reference/is.na.piar_index.md),
[`levels.piar_index()`](https://marberts.github.io/piar/reference/levels.piar_index.md),
[`merge.piar_index()`](https://marberts.github.io/piar/reference/merge.piar_index.md),
[`split.piar_index()`](https://marberts.github.io/piar/reference/split.piar_index.md),
[`stack.piar_index()`](https://marberts.github.io/piar/reference/stack.piar_index.md),
[`time.piar_index()`](https://marberts.github.io/piar/reference/time.piar_index.md),
[`window.piar_index()`](https://marberts.github.io/piar/reference/window.piar_index.md)

## Examples

``` r
index <- as_index(matrix(c(1:12, 12:1), 2, byrow = TRUE), chainable = FALSE)

# Turn a monthly index into a quarterly index
mean(index, window = 3)
#> Fixed-base price index for 2 levels over 4 time periods 
#>       time
#> levels  1 4 7 10
#>      1  2 5 8 11
#>      2 11 8 5  2
```
