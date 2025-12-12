# Missing values in a price index

Identify missing values in a price index.

## Usage

``` r
# S3 method for class 'piar_index'
is.na(x)

# S3 method for class 'piar_index'
anyNA(x, recursive = FALSE)
```

## Arguments

- x:

  A price index, as made by, e.g.,
  [`elementary_index()`](https://marberts.github.io/piar/reference/elementary_index.md).

- recursive:

  Check if `x` also has missing percent-change contributions. By default
  only index values are checked for missingness.

## Value

[`is.na()`](https://rdrr.io/r/base/NA.html) returns a logical matrix,
with a row for each level of `x` and a columns for each time period,
that indicates which index values are missing.

[`anyNA()`](https://rdrr.io/r/base/NA.html) returns `TRUE` if any index
values are missing, or percent-change contributions (if
`recursive = TRUE`).

## See also

Other index methods:
[`[.piar_index()`](https://marberts.github.io/piar/reference/sub-.piar_index.md),
[`aggregate.piar_index`](https://marberts.github.io/piar/reference/aggregate.piar_index.md),
[`as.data.frame.piar_index()`](https://marberts.github.io/piar/reference/as.data.frame.piar_index.md),
[`as.ts.piar_index()`](https://marberts.github.io/piar/reference/as.ts.piar_index.md),
[`chain()`](https://marberts.github.io/piar/reference/chain.md),
[`contrib()`](https://marberts.github.io/piar/reference/contrib.md),
[`head.piar_index()`](https://marberts.github.io/piar/reference/head.piar_index.md),
[`levels.piar_index()`](https://marberts.github.io/piar/reference/levels.piar_index.md),
[`mean.piar_index`](https://marberts.github.io/piar/reference/mean.piar_index.md),
[`merge.piar_index()`](https://marberts.github.io/piar/reference/merge.piar_index.md),
[`split.piar_index()`](https://marberts.github.io/piar/reference/split.piar_index.md),
[`stack.piar_index()`](https://marberts.github.io/piar/reference/stack.piar_index.md),
[`time.piar_index()`](https://marberts.github.io/piar/reference/time.piar_index.md),
[`window.piar_index()`](https://marberts.github.io/piar/reference/window.piar_index.md)

## Examples

``` r
index <- as_index(matrix(c(1, 2, 3, NA, 5, NA), 2))

anyNA(index)
#> [1] TRUE
is.na(index)
#>       time
#> levels     1     2     3
#>      1 FALSE FALSE FALSE
#>      2 FALSE  TRUE  TRUE

# Carry forward imputation

index[is.na(index)] <- 1
index
#> Period-over-period price index for 2 levels over 3 time periods 
#>       time
#> levels 1 2 3
#>      1 1 3 5
#>      2 2 1 1
```
