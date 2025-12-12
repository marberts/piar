# Get the weights for an aggregation structure

Get and set the weights for a price index aggregation structure.

## Usage

``` r
# S3 method for class 'piar_aggregation_structure'
weights(object, ..., ea_only = TRUE, na.rm = FALSE)

weights(object) <- value

# S3 method for class 'piar_aggregation_structure'
weights(object) <- value

set_weights(object, value)
```

## Arguments

- object:

  A price index aggregation structure, as made by
  [`aggregation_structure()`](https://marberts.github.io/piar/reference/aggregation_structure.md).

- ...:

  Not currently used.

- ea_only:

  Should weights be returned for only the elementary aggregates (the
  default)? Setting to `FALSE` gives the weights for the entire
  aggregation structure.

- na.rm:

  Should missing values be removed when aggregating the weights (i.e.,
  when `ea_only = FALSE`)? By default, missing values are not removed.

- value:

  A numeric vector of weights for the elementary aggregates of `object`.

## Value

[`weights()`](https://rdrr.io/r/stats/weights.html) returns a named
vector of weights for the elementary aggregates. The replacement method
replaces these values without changing the aggregation structure.
(`set_weights()` is an alias that's easier to use with pipes.)

If `ea_only = FALSE` then the return value is a list with a named vector
of weights for each level in the aggregation structure.

## See also

Other aggregation structure methods:
[`as.matrix.piar_aggregation_structure()`](https://marberts.github.io/piar/reference/as.matrix.piar_aggregation_structure.md),
[`cut.piar_aggregation_structure()`](https://marberts.github.io/piar/reference/cut.piar_aggregation_structure.md),
[`levels.piar_aggregation_structure()`](https://marberts.github.io/piar/reference/levels.piar_aggregation_structure.md),
[`update.piar_aggregation_structure()`](https://marberts.github.io/piar/reference/update.piar_aggregation_structure.md)

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

# Extract the weights

weights(pias)
#> 111 112 121 
#>   1   3   4 

# ... or update them

weights(pias) <- 1:3
weights(pias)
#> 111 112 121 
#>   1   2   3 
```
