# Extract index values

Methods to extract and replace index values like a matrix.

## Usage

``` r
# S3 method for class 'piar_index'
x[i, j, ...]

# S3 method for class 'piar_index'
x[i, j, ...] <- value
```

## Arguments

- x:

  A price index, as made by, e.g.,
  [`elementary_index()`](https://marberts.github.io/piar/reference/elementary_index.md).

- i, j:

  Indices for the levels and time periods of a price index. See details.

- ...:

  Not currently used.

- value:

  A numeric vector, price index, or list of price indexes. See details.

## Value

A price index that inherits from the same class as `x`.

## Details

The extraction method treats `x` like a matrix of index values with
(named) rows for each level and columns for each time period in `x`.
Unlike a matrix, dimensions are never dropped as subscripting `x` always
returns an index object. The one exception is when subscripting with a
matrix, in which case a list of index values is returned. As `x` is not
an atomic vector, subscripting with a single index like `x[1]` extracts
all time periods for that level.

The replacement method similarly treat `x` like a matrix. If `value` is
an index object with the same number of time periods as `x[i, j]` and it
inherits from the same class as `x`, then the index values and
percent-change contributions of `x[i, j]` are replaced with those for
the corresponding levels of `value`. If `value` is not an index, then it
is coerced to a numeric vector and behaves the same as replacing values
in a matrix. Note that replacing the values of an index will remove the
corresponding percent-change contributions (if any). When replacing with
a matrix, `value` can be a list of index objects.

## See also

Other index methods:
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
[`stack.piar_index()`](https://marberts.github.io/piar/reference/stack.piar_index.md),
[`time.piar_index()`](https://marberts.github.io/piar/reference/time.piar_index.md),
[`window.piar_index()`](https://marberts.github.io/piar/reference/window.piar_index.md)

## Examples

``` r
index <- as_index(matrix(1:6, 2))

index["1", ]
#> Period-over-period price index for 1 levels over 3 time periods 
#>       time
#> levels 1 2 3
#>      1 1 3 5

index[, 2]
#> Period-over-period price index for 2 levels over 1 time periods 
#>       time
#> levels 2
#>      1 3
#>      2 4

index[1, ] <- 1 # can be useful for doing specific imputations

index
#> Period-over-period price index for 2 levels over 3 time periods 
#>       time
#> levels 1 2 3
#>      1 1 1 1
#>      2 2 4 6
```
