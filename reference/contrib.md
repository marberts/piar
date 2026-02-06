# Extract percent-change contributions

Extract a matrix or data frame of percent-change contributions from a
price index.

## Usage

``` r
contrib(x, level = NULL, period = NULL, pad = 0)

contrib2DF(x, levels = NULL, period = NULL)

contrib(x, level = NULL, period = NULL) <- value

set_contrib(x, level = NULL, period = NULL, value)

set_contrib_from_index(x)
```

## Arguments

- x:

  A price index, as made by, e.g.,
  [`elementary_index()`](https://marberts.github.io/piar/reference/elementary_index.md).

- level, levels:

  The level of an index for which percent-change contributions are
  desired, defaulting to the first level (usually the top-level for an
  aggregate index). `contrib2DF()` can accept multiple levels.

- period:

  The time periods for which percent-change contributions are desired,
  defaulting to all time periods.

- pad:

  A numeric value to pad contributions so that they fit into a
  rectangular array when products differ over time. The default is 0.

- value:

  A numeric matrix of replacement contributions with a row for each
  product and a column for each time period. Recycling occurs along time
  periods.

## Value

`contrib()` returns a matrix of percent-change contributions with a
column for each `period` and a row for each product (sorted) for which
there are contributions in `level`. Contributions are padded with `pad`
to fit into a rectangular array when products differ over time. The
replacement methods returns a copy of `x` with contributions given by
the matrix `value`. (`set_contrib()` is an alias that's easier to use
with pipes.) `set_contrib_from_index()` is a helper to return a copy of
`x` with all contributions set to the corresponding index value minus 1.

`contrib2DF()` returns a data frame of contributions with four columns:
`period`, `level`, `product`, and `value`.

## See also

Other index methods:
[`[.piar_index()`](https://marberts.github.io/piar/reference/sub-.piar_index.md),
[`aggregate.piar_index`](https://marberts.github.io/piar/reference/aggregate.piar_index.md),
[`as.data.frame.piar_index()`](https://marberts.github.io/piar/reference/as.data.frame.piar_index.md),
[`as.ts.piar_index()`](https://marberts.github.io/piar/reference/as.ts.piar_index.md),
[`chain()`](https://marberts.github.io/piar/reference/chain.md),
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

index <- elementary_index(prices, rel ~ period + ea, contrib = TRUE)

pias <- aggregation_structure(
  list(c("top", "top", "top"), c("a", "b", "c")),
  weights = 1:3
)

index <- aggregate(index, pias, na.rm = TRUE)

# Percent-change contributions for the top-level index

contrib(index)
#>        time
#> product         1         2
#>     a.1 0.0000000 0.5081686
#>     a.2 0.2440169 0.6442213
#>     b.1 0.3905243 2.0513858
#>     b.2 0.8284271 2.4871732

contrib2DF(index)
#>   period level product     value
#> 1      1   top     a.1 0.0000000
#> 2      1   top     a.2 0.2440169
#> 3      1   top     b.1 0.3905243
#> 4      1   top     b.2 0.8284271
#> 5      2   top     a.1 0.5081686
#> 6      2   top     a.2 0.6442213
#> 7      2   top     b.1 2.0513858
#> 8      2   top     b.2 2.4871732

# Calculate EA contributions for the chained index

library(gpindex)

arithmetic_contributions(
  as.matrix(chain(index))[c("a", "b", "c"), 2],
  weights(pias)
)
#>        a        b        c 
#> 1.541158 6.198639 7.739798 
```
