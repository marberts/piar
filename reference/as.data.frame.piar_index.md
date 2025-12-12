# Coerce an index into a tabular form

Turn an index into a data frame or a matrix.

## Usage

``` r
# S3 method for class 'piar_index'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  contrib = FALSE,
  stringsAsFactors = FALSE
)

# S3 method for class 'piar_index'
as.matrix(x, ...)
```

## Arguments

- x:

  A price index, as made by, e.g.,
  [`elementary_index()`](https://marberts.github.io/piar/reference/elementary_index.md).

- row.names, stringsAsFactors:

  See [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

- optional:

  Not currently used.

- ...:

  Not currently used.

- contrib:

  Include percent-change contributions (the default does not include
  them).

## Value

[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
the index values in `x` as a data frame with three columns: `period`,
`level`, and `value`. If `contrib = TRUE` then there is a fourth (list)
column `contrib` containing percent-change contributions.

[`as.matrix()`](https://rdrr.io/r/base/matrix.html) returns the index
values in `x` as a matrix with a row for each level and a column for
each time period in `x`.

## See also

[`as_index()`](https://marberts.github.io/piar/reference/as_index.md) to
coerce a matrix/data frame of index values into an index object.

Other index methods:
[`[.piar_index()`](https://marberts.github.io/piar/reference/sub-.piar_index.md),
[`aggregate.piar_index`](https://marberts.github.io/piar/reference/aggregate.piar_index.md),
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

as.data.frame(index)
#>   period level value
#> 1      1     1     1
#> 2      1     2     2
#> 3      2     1     3
#> 4      2     2     4
#> 5      3     1     5
#> 6      3     2     6

as.matrix(index)
#>       time
#> levels 1 2 3
#>      1 1 3 5
#>      2 2 4 6
```
