# Transmute weights for a nested generalized mean

Transmute weights to turn a nested generalized mean of a given order
into a generalized mean of any order. Useful for calculating additive
and multiplicative decompositions for an index made of nested
generalized means (e.g., Fisher index). See
[`vignette("decomposing-indexes")`](https://marberts.github.io/piar/articles/decomposing-indexes.md)
for details.

## Usage

``` r
transmute_weights2(
  x,
  weights = list(NULL, NULL),
  order = c(1, -1),
  outer_weights = NULL,
  outer_order = 0,
  to = 1,
  pivot = outer_order
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

- to:

  A finite number giving the order of the target generalized mean for
  the transmuted weights. The default constructs weights for an
  arithmetic mean.

- pivot:

  A finite number giving the pivot value for the transmuted weights. The
  default uses the order of the outer generalized mean, otherwise `to`
  is common alternative.

## Value

A numeric vector, the same length as `x`, that sums to 1.

## Details

This function generalizes the additive and multiplicative decompositions
for the Fisher index by Balk (2008, Chapter 4). It returns a value such
that

    nested_gmean(x, list(w1, w2), c(r1, r2)) ==
        gmean(x, transmute_weights2(x, list(w1, w2), c(r1, r2), to = s), s)

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
[`transmute_weights()`](https://marberts.github.io/piar/reference/transmute_weights.md),
[`update_weights()`](https://marberts.github.io/piar/reference/update_weights.md)

## Examples

``` r
x <- 1:3
w1 <- 3:1
w2 <- c(1, 2, 1)

# Calculate the geometric mean of the arithmetic and harmonic means
# as an arithmetic mean.
nested_gmean(x, list(w1, w2))
#> [1] 1.690309
gmean(x, transmute_weights2(x, list(w1, w2), to = 1))
#> [1] 1.690309
```
