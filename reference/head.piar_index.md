# Return the first/last parts of an index

Extract the first/last parts of an index as if it were a matrix.

## Usage

``` r
# S3 method for class 'piar_index'
head(x, n = 6L, ...)

# S3 method for class 'piar_index'
tail(x, n = 6L, ...)
```

## Arguments

- x:

  A price index, as made by, e.g.,
  [`elementary_index()`](https://marberts.github.io/piar/reference/elementary_index.md).

- n:

  See
  [`head()`](https://rdrr.io/r/utils/head.html)/[`tail()`](https://rdrr.io/r/utils/head.html).
  The default takes the first/last 6 levels of `x`.

- ...:

  Not currently used.

## Value

A price index that inherits from the same class as `x`.

## See also

Other index methods:
[`[.piar_index()`](https://marberts.github.io/piar/reference/sub-.piar_index.md),
[`aggregate.piar_index`](https://marberts.github.io/piar/reference/aggregate.piar_index.md),
[`as.data.frame.piar_index()`](https://marberts.github.io/piar/reference/as.data.frame.piar_index.md),
[`as.ts.piar_index()`](https://marberts.github.io/piar/reference/as.ts.piar_index.md),
[`chain()`](https://marberts.github.io/piar/reference/chain.md),
[`contrib()`](https://marberts.github.io/piar/reference/contrib.md),
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
index <- as_index(matrix(1:9, 3))

head(index, 1)
#> Period-over-period price index for 1 levels over 3 time periods 
#>       time
#> levels 1 2 3
#>      1 1 4 7

tail(index, 1)
#> Period-over-period price index for 1 levels over 3 time periods 
#>       time
#> levels 1 2 3
#>      3 3 6 9
```
