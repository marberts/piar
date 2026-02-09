# Window a price index

Extract and replace index values over a window of time periods.

## Usage

``` r
# S3 method for class 'piar_index'
window(x, start = NULL, end = NULL, ...)

# S3 method for class 'piar_index'
window(x, start = NULL, end = NULL, ...) <- value
```

## Arguments

- x:

  A price index, as made by, e.g.,
  [`elementary_index()`](https://marberts.github.io/piar/reference/elementary_index.md).

- start:

  The time period to start the window. The default in the first period
  of `x`.

- end:

  The time period to end the window. The default is the last period of
  `x`.

- ...:

  Not currently used.

- value:

  A numeric vector or price index.

## Value

[`window()`](https://rdrr.io/r/stats/window.html) extracts a price index
over a window of time periods that inherits from the same class as `x`.
The replacement method replaces these with `value`.

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
[`stack.piar_index()`](https://marberts.github.io/piar/reference/stack.piar_index.md),
[`time.piar_index()`](https://marberts.github.io/piar/reference/time.piar_index.md)

## Examples

``` r
x <- as_index(matrix(1:9, 3))

window(x, "2")
#> Period-over-period price index for 3 levels over 2 time periods 
#>       time
#> levels 2 3
#>      1 4 7
#>      2 5 8
#>      3 6 9

window(x, "2") <- 1
x
#> Period-over-period price index for 3 levels over 3 time periods 
#>       time
#> levels 1 2 3
#>      1 1 1 1
#>      2 2 1 1
#>      3 3 1 1
```
