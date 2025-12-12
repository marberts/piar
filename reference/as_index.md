# Coerce to a price index

Coerce pre-computed index values into an index object.

## Usage

``` r
as_index(x, ...)

# Default S3 method
as_index(x, ...)

# S3 method for class 'matrix'
as_index(x, ..., chainable = TRUE, contrib = FALSE)

# S3 method for class 'data.frame'
as_index(x, ..., contrib = FALSE)

# S3 method for class 'chainable_piar_index'
as_index(x, ..., chainable = TRUE)

# S3 method for class 'direct_piar_index'
as_index(x, ..., chainable = FALSE)

# S3 method for class 'mts'
as_index(x, ...)
```

## Arguments

- x:

  An object to coerce into a price index.

- ...:

  Further arguments passed to or used by methods.

- chainable:

  Are the index values in `x` period-over-period indexes, suitable for a
  chained calculation (the default)? This should be `FALSE` when `x`
  contains fixed-base (direct) index values.

- contrib:

  Should the index values in `x` be used to construct percent-change
  contributions? The default does not make contributions.

## Value

A price index that inherits from
[`piar_index`](https://marberts.github.io/piar/reference/piar_index.md).
If `chainable = TRUE` then this is a period-over-period price index that
also inherits from
[`chainable_piar_index`](https://marberts.github.io/piar/reference/piar_index.md);
otherwise, it is a fixed-base index that inherits from
[`direct_piar_index`](https://marberts.github.io/piar/reference/piar_index.md).

## Details

Numeric matrices are coerced into an index object by treating each
column as a separate time period, and each row as a separate level of
the index (e.g., an elementary aggregate). Column names are used to
denote time periods, and row names are used to denote levels (so they
must be unique). This essentially reverses calling
[`as.matrix()`](https://marberts.github.io/piar/reference/as.data.frame.piar_index.md)
on an index object. If a dimension is unnamed, then it is given a
sequential label from 1 to the size of that dimension. The default and
multiple time series methods coerces `x` to a matrix prior to using the
matrix method.

The data frame method for `as_index()` is best understood as reversing
the effect of
[`as.data.frame()`](https://marberts.github.io/piar/reference/as.data.frame.piar_index.md)
on an index object. It constructs a matrix by taking the levels of
`x[[1]]` as columns and the levels of `x[[2]]` as rows (coercing to a
factor if necessary). It then populates this matrix with the
corresponding values in `x[[3]]`, and uses the matrix method for
`as_index()`. If `contrib = TRUE` and there is a fourth list column of
product contributions then these are also included in the resulting
index.

If `x` is a period-over-period index then it is returned unchanged when
`chainable = TRUE` and chained otherwise. Similarly, if `x` is a
fixed-base index then it is returned unchanged when `chainable = FALSE`
and unchain otherwise.

## See also

[`as.matrix()`](https://marberts.github.io/piar/reference/as.data.frame.piar_index.md)
and
[`as.data.frame()`](https://marberts.github.io/piar/reference/as.data.frame.piar_index.md)
for coercing an index into a tabular form.

## Examples

``` r
prices <- data.frame(
  rel = 1:8,
  period = rep(1:2, each = 4),
  ea = rep(letters[1:2], 4)
)

index <- elementary_index(prices, rel ~ period + ea)

all.equal(as_index(as.data.frame(index)), index)
#> [1] TRUE
all.equal(as_index(as.matrix(index)), index)
#> [1] TRUE
```
