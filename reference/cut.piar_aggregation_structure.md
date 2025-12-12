# Cut an aggregation structure

Keep only the part of an aggregation structure above or below a certain
level.

## Usage

``` r
# S3 method for class 'piar_aggregation_structure'
cut(x, level, ..., na.rm = FALSE, upper = TRUE)
```

## Arguments

- x:

  A price index aggregation structure, as made by
  [`aggregation_structure()`](https://marberts.github.io/piar/reference/aggregation_structure.md).

- level:

  A positive integer, or something that can be coerced into one, giving
  the level at which to cut `x`.

- ...:

  Not currently used.

- na.rm:

  Should missing values be removed when aggregating the weights? By
  default, missing values are not removed.

- upper:

  Keep only the part of `x` above `level` (the default); otherwise,
  return the part of `x` below `level`.

## Value

A price index aggregation structure.

## See also

Other aggregation structure methods:
[`as.matrix.piar_aggregation_structure()`](https://marberts.github.io/piar/reference/as.matrix.piar_aggregation_structure.md),
[`levels.piar_aggregation_structure()`](https://marberts.github.io/piar/reference/levels.piar_aggregation_structure.md),
[`update.piar_aggregation_structure()`](https://marberts.github.io/piar/reference/update.piar_aggregation_structure.md),
[`weights.piar_aggregation_structure()`](https://marberts.github.io/piar/reference/weights.piar_aggregation_structure.md)

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

# Turn it into
#            1
#      |-----+-----|
#      11          12
#     (4)         (4)

cut(pias, 2)
#> Aggregation structure for 2 elementary aggregates with 1 levels above the elementary aggregates 
#>   level1 level2 weight
#> 1      1     11      4
#> 2      1     12      4
```
