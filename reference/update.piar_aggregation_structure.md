# Update an aggregation structure

Price update the weights in a price index aggregation structure.

## Usage

``` r
# S3 method for class 'piar_aggregation_structure'
update(object, index, period = end(index), ..., r = 1)
```

## Arguments

- object:

  A price index aggregation structure, as made by
  [`aggregation_structure()`](https://marberts.github.io/piar/reference/aggregation_structure.md).

- index:

  A fixed-base (direct) price index, or something that can be coerced
  into one. Usually an aggregate price index as made by
  [`aggregate()`](https://marberts.github.io/piar/reference/aggregate.piar_index.md).

- period:

  The time period used to price update the weights. The default uses the
  last period in `index`.

- ...:

  Not currently used.

- r:

  Order of the generalized mean to update the weights. The default is 1
  for an arithmetic index.

## Value

A copy of `object` with price-updated weights using the index values in
`index`.

## See also

[`aggregate()`](https://marberts.github.io/piar/reference/aggregate.piar_index.md)
to make an aggregated price index.

Other aggregation structure methods:
[`as.matrix.piar_aggregation_structure()`](https://marberts.github.io/piar/reference/as.matrix.piar_aggregation_structure.md),
[`cut.piar_aggregation_structure()`](https://marberts.github.io/piar/reference/cut.piar_aggregation_structure.md),
[`levels.piar_aggregation_structure()`](https://marberts.github.io/piar/reference/levels.piar_aggregation_structure.md),
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

pias <- as_aggregation_structure(aggregation_weights)

index <- as_index(
  matrix(1:9, 3, dimnames = list(c("111", "112", "121"), NULL))
)

weights(pias, ea_only = FALSE)
#> $level1
#> 1 
#> 8 
#> 
#> $level2
#> 11 12 
#>  4  4 
#> 
#> $ea
#> 111 112 121 
#>   1   3   4 
#> 

weights(update(pias, index), ea_only = FALSE)
#> $level1
#>   1 
#> 916 
#> 
#> $level2
#>  11  12 
#> 268 648 
#> 
#> $ea
#> 111 112 121 
#>  28 240 648 
#> 
```
