# Update weights

Factor weights to turn the generalized mean of a product into the
product of generalized means. Useful for price-updating the weights in a
generalized-mean index.

## Usage

``` r
update_weights(x, weights = NULL, order = 1)
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

## Value

A numeric vector the same length as `x`.

## Details

This function generalizes the result in section C.5 of Chapter 9 of the
PPI Manual for chaining the Young index, and gives a way to chain
generalized-mean price indexes over time. It returns a value such that

    gmean(x * y, w) ==
        gmean(x, w) * gmean(y, update_weights(x, w))

Factoring weights returns a value that is the same length as `x`, so any
missing values in `x` or `weights` will return `NA`. Unless all values
are `NA`, however, the result will still satisfy the above identity when
`na.rm = TRUE`.

## References

ILO, IMF, OECD, UNECE, and World Bank. (2004). *Producer Price Index
Manual: Theory and Practice*. International Monetary Fund.

## See also

Other math functions:
[`emean()`](https://marberts.github.io/piar/reference/emean.md),
[`gmean()`](https://marberts.github.io/piar/reference/gmean.md),
[`nested_gmean()`](https://marberts.github.io/piar/reference/nested_gmean.md),
[`scale_weights()`](https://marberts.github.io/piar/reference/scale_weights.md),
[`transmute_weights()`](https://marberts.github.io/piar/reference/transmute_weights.md),
[`transmute_weights2()`](https://marberts.github.io/piar/reference/transmute_weights2.md)

## Examples

``` r
x <- 1:3
y <- 4:6
w <- 3:1

# Factor the arithmetic mean by chaining the calculation.
gmean(x * y, w)
#> [1] 8.333333
gmean(x, w) * gmean(y, update_weights(x, w))
#> [1] 8.333333

# In cases where x and y have the same order, Chebyshev's
# inequality implies that the chained calculation is too small.
gmean(x * y, w) > gmean(x, w) * gmean(y, w)
#> [1] TRUE
```
