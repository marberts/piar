# Generalized mean

Calculated a weighted generalized mean.

## Usage

``` r
gmean(x, weights = NULL, order = 1, na.rm = FALSE)
```

## Arguments

- x:

  `[numeric > 0]` A strictly positive numeric vector.

- weights:

  `[numeric >= 0]` A positive numeric vector of weights, the same length
  as `x`. The default is to equally weight each element of `x`.

- order:

  `[numeric(1)]` A finite number giving the order of the generalized
  mean. The default calculates an arithmetic mean.

- na.rm:

  `[logical(1)]` Should missing values be removed? By default, missing
  values are not removed.

## Value

A numeric value for the generalized mean.

## Details

The generalized mean is also called the power mean, Hölder mean, or
\\l_p\\ mean; see Bullen (2003, p. 175) for details.

Both `x` and `weights` should be strictly positive (and finite),
especially for the purpose of making a price index. This is not
enforced, but the results may not make sense if the generalized mean is
not defined. There are two exceptions to this.

1.  The convention by Hardy et al. (1952, p. 13) is used in cases where
    `x` has zeros: the generalized mean is 0 whenever the weights are
    strictly positive and `order < 0`. The analogous convention holds
    whenever at least one element of `x` is `Inf`: the generalized mean
    is `Inf` whenever the weights are strictly positive and `order > 0`.

2.  Some authors let the weighs be non-negative and sum to 1. If there
    are zero weights then the corresponding element of `x` has no impact
    on the result whenever `x` is strictly positive. Unlike
    [`weighted.mean()`](https://rdrr.io/r/stats/weighted.mean.html),
    however, zero weights are not strong zeros, so infinite values in
    `x` will propagate.

The weights are scaled to sum to 1 to satisfy the definition of a
generalized mean.

## Note

The generalized mean can be defined on the extended real line, so that
`order = -Inf / Inf` returns
[`min()`](https://rdrr.io/r/base/Extremes.html)/[`max()`](https://rdrr.io/r/base/Extremes.html),
to agree with the definition by Bullen (2003). This is not implemented,
and the order of the generalized mean must be finite.

## References

Bullen, P. S. (2003). *Handbook of Means and Their Inequalities*.
Springer Science+Business Media.

Hardy, G., Littlewood, J. E., and Polya, G. (1952). *Inequalities* (2nd
edition). Cambridge University Press.

## See also

Other math functions:
[`emean()`](https://marberts.github.io/piar/reference/emean.md),
[`nested_gmean()`](https://marberts.github.io/piar/reference/nested_gmean.md),
[`scale_weights()`](https://marberts.github.io/piar/reference/scale_weights.md),
[`transmute_weights()`](https://marberts.github.io/piar/reference/transmute_weights.md),
[`transmute_weights2()`](https://marberts.github.io/piar/reference/transmute_weights2.md),
[`update_weights()`](https://marberts.github.io/piar/reference/update_weights.md)

## Examples

``` r
x <- 1:3
w <- c(0.25, 0.25, 0.5)

# Arithmetic mean.
gmean(x, w)
#> [1] 2.25

# Geometric mean.
gmean(x, w, order = 0)
#> [1] 2.059767

# The Lehmer mean is a generalized mean with specific weights.
gmean(x, w * x)
#> [1] 2.555556
```
