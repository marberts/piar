# Split an index into groups

Split an index into groups of indexes according to a factor, along
either the levels or time periods of the index.

## Usage

``` r
# S3 method for class 'piar_index'
split(x, f, drop = FALSE, ..., along = c("levels", "time"))

# S3 method for class 'piar_index'
split(x, f, drop = FALSE, ..., along = c("levels", "time")) <- value
```

## Arguments

- x:

  A price index, as made by, e.g.,
  [`elementary_index()`](https://marberts.github.io/piar/reference/elementary_index.md).

- f:

  A factor or list of factors to group elements of `x`.

- drop:

  Should levels that do not occur in `f` be dropped? By default all
  levels are kept.

- ...:

  Further arguments passed to
  [`split.default()`](https://rdrr.io/r/base/split.html).

- along:

  Either `"levels"` to split over the levels of `x` (the default), or
  `"time"` to split over the time periods of `x`.

- value:

  A list of values compatible with the splitting of `x`, or something
  that can be coerced into one, recycled if necessary.

## Value

[`split()`](https://rdrr.io/r/base/split.html) returns a list of index
objects for each level in `f`. The replacement method replaces these
values with the corresponding element of `value`.

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
[`stack.piar_index()`](https://marberts.github.io/piar/reference/stack.piar_index.md),
[`time.piar_index()`](https://marberts.github.io/piar/reference/time.piar_index.md),
[`window.piar_index()`](https://marberts.github.io/piar/reference/window.piar_index.md)

## Examples

``` r
index <- as_index(matrix(1:6, 2))

split(index, 1:2)
#> $`1`
#> Period-over-period price index for 1 levels over 3 time periods 
#>       time
#> levels 1 2 3
#>      1 1 3 5
#> 
#> $`2`
#> Period-over-period price index for 1 levels over 3 time periods 
#>       time
#> levels 1 2 3
#>      2 2 4 6
#> 

split(index, c(1, 1, 2), along = "time")
#> $`1`
#> Period-over-period price index for 2 levels over 2 time periods 
#>       time
#> levels 1 2
#>      1 1 3
#>      2 2 4
#> 
#> $`2`
#> Period-over-period price index for 2 levels over 1 time periods 
#>       time
#> levels 3
#>      1 5
#>      2 6
#> 
```
