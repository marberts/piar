# Stack price indexes

[`stack()`](https://rdrr.io/r/utils/stack.html) combines two price
indexes with common levels, stacking index values and percent-change
contributions for one index after the other.

[`unstack()`](https://rdrr.io/r/utils/stack.html) breaks up a price
index into a list of indexes for each time period.

These methods can be used in a map-reduce to make an index with multiple
aggregation structures (like a Paasche index).

## Usage

``` r
# S3 method for class 'chainable_piar_index'
stack(x, y, ...)

# S3 method for class 'direct_piar_index'
stack(x, y, ...)

# S3 method for class 'chainable_piar_index'
unstack(x, ...)

# S3 method for class 'direct_piar_index'
unstack(x, ...)
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

[`stack()`](https://rdrr.io/r/utils/stack.html) returns a combined price
index that inherits from the same class as `x`.

[`unstack()`](https://rdrr.io/r/utils/stack.html) returns a list of
price indexes with the same class as `x`.

## Note

It may be necessary to use
[`rebase()`](https://marberts.github.io/piar/reference/chain.md) prior
to stacking fixed-based price indexes to ensure they have the same base
period.

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
[`merge.piar_index()`](https://marberts.github.io/piar/reference/merge.piar_index.md),
[`split.piar_index()`](https://marberts.github.io/piar/reference/split.piar_index.md),
[`time.piar_index()`](https://marberts.github.io/piar/reference/time.piar_index.md),
[`window.piar_index()`](https://marberts.github.io/piar/reference/window.piar_index.md)

## Examples

``` r
index1 <- as_index(matrix(1:6, 2))

index2 <- index1
time(index2) <- 4:6

stack(index1, index2)
#> Period-over-period price index for 2 levels over 6 time periods 
#>       time
#> levels 1 2 3 4 5 6
#>      1 1 3 5 1 3 5
#>      2 2 4 6 2 4 6

# Unstack does the reverse

all.equal(
  c(unstack(index1), unstack(index2)),
  unstack(stack(index1, index2))
)
#> [1] TRUE
```
