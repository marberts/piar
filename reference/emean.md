# Extended mean

Calculate the component-wise extended mean.

## Usage

``` r
emean(x, y, order = c(0, 1), tol = .Machine$double.eps^0.5)
```

## Arguments

- x, y:

  `[numeric > 0]` A strictly positive numeric vector.

- order:

  `[numeric(2)]` A pair of finite numbers giving the order of the
  extended mean. The default calculates the ordinary logarithmic mean.
  Setting either the first or second element to 1 gives the generalized
  logarithmic mean.

- tol:

  `[numeric > 0]` The tolerance used to determine if `x == y`. The
  default value is the same as
  [`all.equal()`](https://rdrr.io/r/base/all.equal.html).

## Value

A numeric vector, the same length as `max(length(x), length(y))`, giving
the component-wise extended mean of `x` and `y`.

## Details

The extended mean is also called the difference mean, Stolarsky mean, or
extended mean-value mean; see Bullen (2003, p. 393) for details.

Both `x` and `y` should be strictly positive. This is not enforced, but
the results may not make sense when the extended mean is not defined.
The usual recycling rules apply when `x` and `y` are not the same
length.

By definition, the extended mean of `x` and `y` is `x` when `x == y`.
The `tol` argument is used to test equality by checking if
`abs(x - y) <= tol`. In some cases it's useful to multiply `tol` by a
scale factor, such as `max(abs(x), abs(y))`. This often doesn't matter
when making price indexes, however, as `x` and `y` are usually around 1.

## References

Bullen, P. S. (2003). *Handbook of Means and Their Inequalities*.
Springer Science+Business Media.

## See also

Other math functions:
[`gmean()`](https://marberts.github.io/piar/reference/gmean.md),
[`nested_gmean()`](https://marberts.github.io/piar/reference/nested_gmean.md),
[`scale_weights()`](https://marberts.github.io/piar/reference/scale_weights.md),
[`transmute_weights()`](https://marberts.github.io/piar/reference/transmute_weights.md),
[`transmute_weights2()`](https://marberts.github.io/piar/reference/transmute_weights2.md),
[`update_weights()`](https://marberts.github.io/piar/reference/update_weights.md)

## Examples

``` r
x <- 8:5
y <- 1:4

# The arithmetic and geometric means are special cases of the
# generalized logarithmic mean.
all.equal(emean(x, y, c(2, 1)), (x + y) / 2)
#> [1] TRUE
all.equal(emean(x, y, c(-1, 1)), sqrt(x * y))
#> [1] TRUE

# The harmonic mean cannot be expressed as a logarithmic mean, but can
# be expressed as an extended mean.
all.equal(emean(x, y, c(-2, -1)), 2 / (1 / x + 1 / y))
#> [1] TRUE

# The quadratic mean is also a type of extended mean.
all.equal(emean(x, y, c(2, 4)), sqrt(x^2 / 2 + y^2 / 2))
#> [1] TRUE

# As are heronian and centroidal means.
all.equal(
  emean(x, y, c(0.5, 1.5)),
  (x + sqrt(x * y) + y) / 3
)
#> [1] TRUE
all.equal(
  emean(x, y, c(2, 3)),
  2 / 3 * (x^2 + x * y + y^2) / (x + y)
)
#> [1] TRUE
```
