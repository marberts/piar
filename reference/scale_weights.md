# Scale weights

Scale a vector of weights so that they sum to 1.

## Usage

``` r
scale_weights(x)
```

## Arguments

- x:

  `[numeric >= 0]` A positive numeric vector with at least one non-zero
  element.

## Value

A numeric vector that sums to 1. If there are `NA`s in `x` then the
result sums 1 to if these values are removed.

## See also

Other math functions:
[`emean()`](https://marberts.github.io/piar/reference/emean.md),
[`gmean()`](https://marberts.github.io/piar/reference/gmean.md),
[`nested_gmean()`](https://marberts.github.io/piar/reference/nested_gmean.md),
[`transmute_weights()`](https://marberts.github.io/piar/reference/transmute_weights.md),
[`transmute_weights2()`](https://marberts.github.io/piar/reference/transmute_weights2.md),
[`update_weights()`](https://marberts.github.io/piar/reference/update_weights.md)

## Examples

``` r
scale_weights(1:5)
#> [1] 0.06666667 0.13333333 0.20000000 0.26666667 0.33333333

scale_weights(c(1:5, NA))
#> [1] 0.06666667 0.13333333 0.20000000 0.26666667 0.33333333         NA
```
