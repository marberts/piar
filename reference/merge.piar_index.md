# Merge price indexes

Combine two price indexes with common time periods, merging together the
index values and percent-change contributions for each time period.

This is useful for building up an index when different elementary
aggregates come from different sources of data, or use different
index-number formulas.

## Usage

``` r
# S3 method for class 'chainable_piar_index'
merge(x, y, ...)

# S3 method for class 'direct_piar_index'
merge(x, y, ...)
```

## Arguments

- x:

  A price index, as made by, e.g.,
  [`elementary_index()`](https://marberts.github.io/piar/reference/elementary_index.md).

- y:

  A price index, or something that can coerced into one. If `x` is a
  period-over-period index then `y` is coerced into a chainable index;
  otherwise, `y` is coerced into a direct index.

- ...:

  Not currently used.

## Value

A combined price index that inherits from the same class as `x`.

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
[`mean.piar_index`](https://marberts.github.io/piar/reference/mean.piar_index.md),
[`split.piar_index()`](https://marberts.github.io/piar/reference/split.piar_index.md),
[`stack.piar_index()`](https://marberts.github.io/piar/reference/stack.piar_index.md),
[`time.piar_index()`](https://marberts.github.io/piar/reference/time.piar_index.md),
[`window.piar_index()`](https://marberts.github.io/piar/reference/window.piar_index.md)

## Examples

``` r
index1 <- as_index(matrix(1:6, 2))

index2 <- index1
levels(index2) <- 3:4

merge(index1, index2)
#> Period-over-period price index for 4 levels over 3 time periods 
#>       time
#> levels 1 2 3
#>      1 1 3 5
#>      2 2 4 6
#>      3 1 3 5
#>      4 2 4 6
```
