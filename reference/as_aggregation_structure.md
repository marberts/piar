# Coerce to an aggregation structure

Coerce an object into an aggregation structure object.

## Usage

``` r
as_aggregation_structure(x, ...)

# Default S3 method
as_aggregation_structure(x, ..., weights = NULL)

# S3 method for class 'data.frame'
as_aggregation_structure(x, ...)

# S3 method for class 'matrix'
as_aggregation_structure(x, ...)
```

## Arguments

- x:

  An object to coerce into an aggregation structure.

- ...:

  Further arguments passed to or used by methods.

- weights:

  A numeric vector of aggregation weights for the elementary aggregates.
  The default is to give each elementary aggregate the same weight.

## Value

A price index aggregation structure that inherits from
[`piar_aggregation_structure`](https://marberts.github.io/piar/reference/aggregation_structure.md).

## Details

The default method attempts to coerce `x` into a list prior to calling
[`aggregation_structure()`](https://marberts.github.io/piar/reference/aggregation_structure.md).

The data frame and matrix methods treat `x` as a table with a row for
each elementary aggregate, a column of labels for each level in the
aggregation structure, and a column of weights for the elementary
aggregates.

## See also

[`as.matrix()`](https://marberts.github.io/piar/reference/as.matrix.piar_aggregation_structure.md)
and
[`as.data.frame()`](https://marberts.github.io/piar/reference/as.matrix.piar_aggregation_structure.md)
for coercing an aggregation structure into a tabular form.

## Examples

``` r
# A simple aggregation structure
#            1
#      |-----+-----|
#      11          12
#  |---+---|       |
#  111     112     121
#  (1)     (3)     (4)

aggregation_weights <- data.frame(
  level1 = c("1", "1", "1"),
  level2 = c("11", "11", "12"),
  ea     = c("111", "112", "121"),
  weight = c(1, 3, 4)
)

pias <- aggregation_structure(
  aggregation_weights[1:3],
  weights = aggregation_weights[[4]]
)

all.equal(
  pias,
  as_aggregation_structure(aggregation_weights)
)
#> [1] TRUE

all.equal(
  pias,
  as_aggregation_structure(as.matrix(aggregation_weights))
)
#> [1] TRUE
```
