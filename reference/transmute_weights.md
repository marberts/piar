# Transmute weights for a generalized mean

Transmute weight to turn a generalized mean of a given order into a
generalized mean of any other order. Useful for calculating additive and
multiplicative decompositions for generalized-mean indexes. See
[`vignette("decomposing-indexes")`](https://marberts.github.io/piar/articles/decomposing-indexes.md)
for more details.

## Usage

``` r
transmute_weights(x, weights = NULL, order = 0, to = 1, mean = NA)
```

## Arguments

- x:

  `[numeric > 0]` A strictly positive numeric vector.

- weights:

  `[numeric >= 0]` A positive numeric vector of weights, the same length
  as `x`. The default is to equally weight each element of `x`.

- order:

  `[numeric(1)]` A finite number giving the order of the generalized
  mean. The default transmutes the weights for a geometric mean.

- to:

  `[numeric(1)]` A finite number giving the order of the target
  generalized mean. The default computes weights for an arithmetic mean.

- mean:

  `[numeric(1)]` A finite number giving the generalized mean of `x` and
  `weights`, if known. The default computes this values.

## Value

A numeric vector, the same length as `x`, that sums to 1.

## Details

This function generalizes the additive and multiplicative decompositions
for arithmetic and geometric indexes by Balk (2008, Chapter 4). It
returns a value such that

    gmean(x, w, r) == gmean(x, transmute_weights(x, w, r, s), s)

Transmuting weights returns a value that is the same length as `x`, so
any missing values in `x` or `weights` will return `NA`. Unless all
values are `NA`, however, the result will still satisfy the above
identity when `na.rm = TRUE`.

## References

Balk, B. M. (2008). *Price and Quantity Index Numbers*. Cambridge
University Press.

## See also

Other math functions:
[`gmean()`](https://marberts.github.io/piar/reference/gmean.md),
[`nested_gmean()`](https://marberts.github.io/piar/reference/nested_gmean.md),
[`scale_weights()`](https://marberts.github.io/piar/reference/scale_weights.md),
[`transmute_weights2()`](https://marberts.github.io/piar/reference/transmute_weights2.md),
[`update_weights()`](https://marberts.github.io/piar/reference/update_weights.md)

## Examples

``` r
x <- 1:3
w <- 3:1

# Calculate the geometric mean as an arithmetic mean.
gmean(x, order = 0)
#> [1] 1.817121
gmean(x, transmute_weights(x, order = 0, to = 1), order = 1)
#> [1] 1.817121
```
