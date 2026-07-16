# Nested generalized means

Calculate a weighted (outer) generalized mean of two (inner) generalized
means (i.e., crossing means).

## Usage

``` r
nested_gmean(
  x,
  weights = list(NULL, NULL),
  order = c(1, -1),
  outer_weights = NULL,
  outer_order = 0,
  na.rm = FALSE
)
```

## Arguments

- x:

  `[numeric > 0]` A strictly positive numeric vector.

- weights:

  `[list]` A list of positive numeric vector of weights, each the same
  length as `x`, for both of the inner generalized means. `NULL`
  elements of `weights` equally weight each element of `x`. The default
  uses equal weights for both inner generalized mean.

- order:

  `[numeric(2)]` A finite numeric vector giving the order of each of the
  inner generalized means. The default computes an arithmetic mean and a
  harmonic mean.

- outer_weights:

  `[numeric(2)]` A strictly positive numeric vector weights for each of
  the inner generalized means as used in the outer generalized mean. The
  default weights each inner generalized mean equally.

- outer_order:

  `[numeric(1)]` A finite number giving the order of the outer
  generalized mean. The default uses a geometric mean.

- na.rm:

  `[logical(1)]` Should missing values in `x` and `weights` be removed?
  By default missing values are not removed. Note that removal of
  missing values is balanced across `x` and both elements of `weights`.

## Value

A numeric value for the nested generalized mean.

## See also

Other math functions:
[`gmean()`](https://marberts.github.io/piar/reference/gmean.md),
[`scale_weights()`](https://marberts.github.io/piar/reference/scale_weights.md),
[`transmute_weights()`](https://marberts.github.io/piar/reference/transmute_weights.md),
[`transmute_weights2()`](https://marberts.github.io/piar/reference/transmute_weights2.md),
[`update_weights()`](https://marberts.github.io/piar/reference/update_weights.md)

## Examples

``` r
x <- 1:3
w1 <- c(0.25, 0.25, 0.5)
w2 <- c(0.3, 0.3, 0.4)
# Calculate the geometric mean of the arithmetic and harmonic means
# to make a Fisher index.
nested_gmean(x, list(w1, w2))
#> [1] 1.963961
```
