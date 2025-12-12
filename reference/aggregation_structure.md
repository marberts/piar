# Make a price index aggregation structure

Create a price index aggregation structure from a hierarchical
classification and aggregation weights that can be used to aggregate
elementary indexes.

## Usage

``` r
aggregation_structure(x, weights = NULL)
```

## Arguments

- x:

  A list of character vectors that give the codes/labels for each level
  of the classification, ordered so that moving down the list goes down
  the hierarchy. The last vector gives the elementary aggregates, which
  should have no duplicates. All vectors should be the same length,
  without `NA`s, and there should be no duplicates across different
  levels of `x`. Names for `x` are used as level names; otherwise,
  levels are named 'level1', 'level2', ..., 'ea'.

- weights:

  A numeric vector of aggregation weights for the elementary aggregates
  (i.e., the last vector in `x`), or something that can be coerced into
  one. The default is to give each elementary aggregate the same weight.

## Value

A price index aggregation structure of class
`piar_aggregation_structure`. This is a list-S3 class with the following
components.

- child:

  A nested list that gives the positions of the immediate children for
  each node in each level of the aggregation structure above the
  terminal nodes.

- parent:

  A list that gives the position of the immediate parent for each node
  of the aggregation structure below the initial nodes.

- levels:

  A named list of character vectors that give the levels of `x`.

- weights:

  A vector giving the weight for each elementary aggregate.

## Warning

The `aggregation_structure()` function does its best to check its
arguments, but there should be no expectation that the result of
`aggregation_structure()` will make any sense if `x` does not represent
a nested hierarchy.

## See also

[`aggregate()`](https://marberts.github.io/piar/reference/aggregate.piar_index.md)
to aggregate price indexes made with
[`elementary_index()`](https://marberts.github.io/piar/reference/elementary_index.md).

[`expand_classification()`](https://marberts.github.io/piar/reference/expand_classification.md)
to make `x` from a character representation of a hierarchical
aggregation structure.

[`as_aggregation_structure()`](https://marberts.github.io/piar/reference/as_aggregation_structure.md)
to coerce tabular data into an aggregation structure.

[`as.data.frame()`](https://marberts.github.io/piar/reference/as.matrix.piar_aggregation_structure.md)
and
[`as.matrix()`](https://marberts.github.io/piar/reference/as.matrix.piar_aggregation_structure.md)
to coerce an aggregation structure into a tabular form.

[`weights()`](https://marberts.github.io/piar/reference/weights.piar_aggregation_structure.md)
to get the weights for an aggregation structure.

[`update()`](https://marberts.github.io/piar/reference/update.piar_aggregation_structure.md)
for updating a price index aggregation structure with an aggregated
index.

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

aggregation_structure(
  aggregation_weights[1:3],
  weights = aggregation_weights[[4]]
)
#> Aggregation structure for 3 elementary aggregates with 2 levels above the elementary aggregates 
#>   level1 level2  ea weight
#> 1      1     11 111      1
#> 2      1     11 112      3
#> 3      1     12 121      4

# The aggregation structure can also be made by expanding the
# elementary aggregates

with(
  aggregation_weights,
  aggregation_structure(expand_classification(ea), weight)
)
#> Aggregation structure for 3 elementary aggregates with 2 levels above the elementary aggregates 
#>   level1 level2  ea weight
#> 1      1     11 111      1
#> 2      1     11 112      3
#> 3      1     12 121      4
```
