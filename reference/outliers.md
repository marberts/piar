# Outlier detection for price relatives

Standard cutoff-based methods for detecting outliers with price
relatives.

## Usage

``` r
outliers(
  x,
  upper,
  lower = upper,
  method = c("quartile", "resistant-fences", "kimber", "robust-z", "tukey"),
  scale = 0,
  quantile_type = 7
)

hb_transform(x)
```

## Arguments

- x:

  `[numeric > 0]` A numeric vector, usually of price relatives. These
  can be made with, e.g.,
  [`back_period()`](https://marberts.github.io/piar/reference/back_period.md).

- upper, lower:

  `[numeric(1) > 0]` A number giving the upper and lower cutoffs for
  each element of `x`. By default the lower cutoff is the same as the
  upper cutoff

- method:

  `[character(1)]` The outlier detection method, one of `"quartile"`
  (the default), `"resistant-fences"`, `"kimber"`, `"robust-z"`, or
  `"tukey"`.

- scale:

  `[0 <= numeric(1) <= 1]` A number between 0 and 1 giving the scale
  factor for the median to establish the minimum dispersion between
  quartiles for each element of `x`. The default does not set a minimum
  dispersion.

- quantile_type:

  `[integer(1)]` See
  [`quantile()`](https://rdrr.io/r/stats/quantile.html).

## Value

A logical vector, the same length as `x`, that is `TRUE` if the
corresponding element of `x` is identified as an outlier, `FALSE`
otherwise.

## Details

This function constructs an interval of the form \\\[b_l(x) - c_l \times
l(x), b_u(x) + c_u \times u(x)\]\\ and assigns a value in `x` as `TRUE`
if that value does not belong to the interval, `FALSE` otherwise. The
different methods differ in how they construct the values \\b_l(x)\\,
\\b_u(x)\\, \\l(x)\\, and \\u(x)\\. Any missing values in `x` are
ignored when calculating the interval, but will return `NA`.

The quartile method and Tukey algorithm are described in paragraphs
5.113 to 5.135 of the CPI manual (2020). The resistant fences method is
an alternative to the quartile method, and is described by Rais (2008)
and Hutton (2008). The Kimber method is yet another alternative.
Quantile-based methods often identify price relatives as outliers
because the distribution is concentrated around 1; setting `scale > 0`
puts a floor on the minimum dispersion between quantiles as a fraction
of the median. See the references for more details.

|  |  |  |  |  |
|----|----|----|----|----|
|  | \\b_l(x)\\ | \\b_u(x)\\ | \\l(x)\\ | \\u(x)\\ |
| Quartile | \\Q_2(x)\\ | \\Q_2(x)\\ | \\Q_2(x) - Q_1(x)\\ | \\Q_3(x) - Q_2(x)\\ |
| Resistant fences | \\Q_1(x)\\ | \\Q_3(x)\\ | \\Q_3(x) - Q_1(x)\\ | \\Q_3(x) - Q_1(x)\\ |
| Kimber | \\Q_1(x)\\ | \\Q_3(x)\\ | \\Q_2(x) - Q_1(x)\\ | \\Q_3(x) - Q_2(x)\\ |

The robust Z-score is the usual method to identify relatives in the
(asymmetric) tails of the distribution, simply replacing the mean with
the median, and the standard deviation with the median absolute
deviation.

These methods often assume that price relatives are symmetrically
distributed (if not Gaussian). As the distribution of price relatives
often has a long right tail, the natural logarithm can be used to
transform price relative before identifying outliers (sometimes under
the assumption that price relatives are distributed log-normal). The
Hidiroglou-Berthelot transformation is another approach, described in
the CPI manual (par. 5.124). (Sometimes the transformed price relatives
are multiplied by \\\max(p_1, p_0)^u\\, for some \\0 \le u \le 1\\, so
that products with a larger price get flagged as outliers (par. 5.128).)

## References

Hutton, H. (2008). Dynamic outlier detection in price index surveys.
*Proceedings of the Survey Methods Section: Statistical Society of
Canada Annual Meeting*.

IMF, ILO, Eurostat, UNECE, OECD, and World Bank. (2020). *Consumer Price
Index Manual: Concepts and Methods*. International Monetary Fund.

Rais, S. (2008). Outlier detection for the Consumer Price Index.
*Proceedings of the Survey Methods Section: Statistical Society of
Canada Annual Meeting*.

## See also

[`back_period()`](https://marberts.github.io/piar/reference/back_period.md)
for a simple utility function to turn prices in a table into price
relatives.

The `HBmethod()` function in the univOutl package for the
Hidiroglou-Berthelot method for identifying outliers.

## Examples

``` r
x <- c(1, 10, 15, 100)

outliers(x, upper = 2.5, method = "quartile")
#> [1] FALSE FALSE FALSE  TRUE

# Always identifies fewer outliers than above.
outliers(x, upper = 2.5, method = "resistant-fences")
#> [1] FALSE FALSE FALSE FALSE
```
