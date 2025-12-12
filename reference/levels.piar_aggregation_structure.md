# Get the levels for an aggregation structure

Get the hierarchical list of levels for an aggregation structure. It is
an error to try and replace these values.

## Usage

``` r
# S3 method for class 'piar_aggregation_structure'
levels(x)
```

## Arguments

- x:

  A price index aggregation structure, as made by
  [`aggregation_structure()`](https://marberts.github.io/piar/reference/aggregation_structure.md).

## Value

A list of character vectors giving the levels for each position in the
aggregation structure.

## See also

Other aggregation structure methods:
[`as.matrix.piar_aggregation_structure()`](https://marberts.github.io/piar/reference/as.matrix.piar_aggregation_structure.md),
[`cut.piar_aggregation_structure()`](https://marberts.github.io/piar/reference/cut.piar_aggregation_structure.md),
[`update.piar_aggregation_structure()`](https://marberts.github.io/piar/reference/update.piar_aggregation_structure.md),
[`weights.piar_aggregation_structure()`](https://marberts.github.io/piar/reference/weights.piar_aggregation_structure.md)
