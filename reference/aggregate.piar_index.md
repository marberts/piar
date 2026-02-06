# Aggregate elementary price indexes

Aggregate elementary price indexes with a price index aggregation
structure.

## Usage

``` r
# S3 method for class 'chainable_piar_index'
aggregate(
  x,
  pias,
  ...,
  pias2 = NULL,
  na.rm = FALSE,
  contrib = TRUE,
  r = 1,
  include_ea = TRUE,
  duplicate_contrib = c("sum", "make.unique")
)

# S3 method for class 'direct_piar_index'
aggregate(
  x,
  pias,
  ...,
  pias2 = NULL,
  na.rm = FALSE,
  contrib = TRUE,
  r = 1,
  include_ea = TRUE,
  duplicate_contrib = c("sum", "make.unique")
)
```

## Arguments

- x:

  A price index, usually made by
  [`elementary_index()`](https://marberts.github.io/piar/reference/elementary_index.md).

- pias:

  A price index aggregation structure or something that can be coerced
  into one. This can be made with
  [`aggregation_structure()`](https://marberts.github.io/piar/reference/aggregation_structure.md).

- ...:

  Not currently used.

- pias2:

  An optional secondary aggregation structure, usually with
  current-period weights, to make a superlative index. See details.

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
  for details. If `pias2` is given then the index is aggregated with a
  quadratic mean of order `2*r`.

- include_ea:

  Should indexes for the elementary aggregates be included along with
  the aggregated indexes? By default, all index values are returned.

- duplicate_contrib:

  The method to deal with duplicate product contributions. Either
  `"make.unique"` to treat duplicate products as distinct products and
  make their names unique with
  [`make.unique()`](https://rdrr.io/r/base/make.unique.html) or `"sum"`
  to add contributions for each product (the default).

## Value

An aggregate price index that inherits from the class of `x`.

## Details

The [`aggregate()`](https://rdrr.io/r/stats/aggregate.html) method loops
over each time period in `x` and

1.  aggregates the elementary indexes with
    [`gpindex::generalized_mean(r)()`](https://marberts.github.io/gpindex/reference/generalized_mean.html)
    for each level of `pias`;

2.  aggregates percent-change contributions for each level of `pias` (if
    there are any and `contrib = TRUE`);

3.  price updates the weights in `pias` with
    [`gpindex::factor_weights(r)()`](https://marberts.github.io/gpindex/reference/factor_weights.html)
    (only for period-over-period elementary indexes).

The result is a collection of aggregated period-over-period indexes that
can be chained together to get a fixed-base index when `x` are
period-over-period elementary indexes. Otherwise, when `x` are
fixed-base elementary indexes, the result is a collection of aggregated
fixed-base (direct) indexes.

By default, missing elementary indexes will propagate when aggregating
the index. Missing elementary indexes can be due to both missingness of
these values in `x`, and the presence of elementary aggregates in `pias`
that are not part of `x`. Setting `na.rm = TRUE` ignores missing values,
and is equivalent to parental (or overall mean) imputation. As an
aggregated price index generally cannot have missing values (for
otherwise it can't be chained over time and weights can't be price
updated), any missing values for a level of `pias` are removed and
recursively replaced by the value of its immediate parent.

In most cases aggregation is done with an arithmetic mean (the default),
and this is detailed in chapter 8 (pp. 190–198) of the CPI manual
(2020), with analogous details in chapter 9 of the PPI manual (2004).
Aggregating with a non-arithmetic mean follows the same steps, except
that the elementary indexes are aggregated with a mean of a different
order (e.g., harmonic for a Paasche index), and the method for price
updating the weights is slightly different. Note that, because
aggregation is done with a generalized mean, the resulting index is
consistent-in-aggregation at each point in time.

Aggregating percent-change contributions uses the method in chapter 9 of
the CPI manual (equations 9.26 and 9.28) when aggregating with an
arithmetic mean. With a non-arithmetic mean, arithmetic weights are
constructed using
[`gpindex::transmute_weights(r, 1)()`](https://marberts.github.io/gpindex/reference/transmute_weights.html)
in order to apply this method.

There may not be contributions for all prices relatives in an elementary
aggregate if the elementary indexes are built from several sources (as
with
[`merge()`](https://marberts.github.io/piar/reference/merge.piar_index.md)).
In this case the contribution for a price relative in the aggregated
index will be correct, but the sum of all contributions will not equal
the change in the value of the index.

If two aggregation structures are given then the steps above are done
for each aggregation structure, with the aggregation for `pias` done
with a generalized mean of order `r` the aggregation for `pias2` done
with a generalized mean of order `-r`. The resulting indexes are
combined with a geometric mean to make a superlative quadratic mean of
order `2*r` index. Percent-change contributions are combined using a
generalized van IJzeren decomposition; see
[`gpindex::nested_transmute()`](https://marberts.github.io/gpindex/reference/transmute_weights.html)
for details.

## Note

For large indexes it can be much faster to turn the aggregation
structure into an aggregation matrix with
[`as.matrix()`](https://marberts.github.io/piar/reference/as.matrix.piar_aggregation_structure.md),
then aggregate elementary indexes as a matrix operation when there are
no missing values. See the examples for details.

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

Other index methods:
[`[.piar_index()`](https://marberts.github.io/piar/reference/sub-.piar_index.md),
[`as.data.frame.piar_index()`](https://marberts.github.io/piar/reference/as.data.frame.piar_index.md),
[`as.ts.piar_index()`](https://marberts.github.io/piar/reference/as.ts.piar_index.md),
[`chain()`](https://marberts.github.io/piar/reference/chain.md),
[`contrib()`](https://marberts.github.io/piar/reference/contrib.md),
[`head.piar_index()`](https://marberts.github.io/piar/reference/head.piar_index.md),
[`is.na.piar_index()`](https://marberts.github.io/piar/reference/is.na.piar_index.md),
[`levels.piar_index()`](https://marberts.github.io/piar/reference/levels.piar_index.md),
[`mean.piar_index`](https://marberts.github.io/piar/reference/mean.piar_index.md),
[`merge.piar_index()`](https://marberts.github.io/piar/reference/merge.piar_index.md),
[`split.piar_index()`](https://marberts.github.io/piar/reference/split.piar_index.md),
[`stack.piar_index()`](https://marberts.github.io/piar/reference/stack.piar_index.md),
[`time.piar_index()`](https://marberts.github.io/piar/reference/time.piar_index.md),
[`window.piar_index()`](https://marberts.github.io/piar/reference/window.piar_index.md)

## Examples

``` r
prices <- data.frame(
  rel = 1:8,
  period = rep(1:2, each = 4),
  ea = rep(letters[1:2], 4)
)

# A two-level aggregation structure

pias <- aggregation_structure(
  list(c("top", "top", "top"), c("a", "b", "c")),
  weights = 1:3
)

# Calculate Jevons elementary indexes

(elementary <- elementary_index(prices, rel ~ period + ea))
#> Period-over-period price index for 2 levels over 2 time periods 
#>       time
#> levels        1        2
#>      a 1.732051 5.916080
#>      b 2.828427 6.928203

# Aggregate (note the imputation for elementary index 'c')

(index <- aggregate(elementary, pias, na.rm = TRUE))
#> Period-over-period price index for 4 levels over 2 time periods 
#>       time
#> levels        1        2
#>    top 2.462968 6.690949
#>    a   1.732051 5.916080
#>    b   2.828427 6.928203
#>    c   2.462968 6.690949

# Aggregation can equivalently be done as matrix multiplication

as.matrix(pias) %*% as.matrix(chain(index[letters[1:3]]))
#>       time
#> levels        1       2
#>    top 2.462968 16.4796
```
