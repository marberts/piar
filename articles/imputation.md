# Imputation

The example in
[`vignette("piar")`](https://marberts.github.io/piar/articles/piar.md)
used parental imputation to both impute missing price relatives when
calculating the elementary indexes and to impute missing elementary
indexes during aggregation. Although parental imputation is simple and
transparent, it is not the only way to impute missing prices or index
values.

## Imputing missing prices

Instead of implicitly imputing missing price relatives by ignoring
missing values, a common explicit (but methodologically dubious)
imputation strategy when making elementary indexes is to carry forward
the previous price to impute for missing prices. As the
[`elementary_index()`](https://marberts.github.io/piar/reference/elementary_index.md)
function accepts price relatives as its input, any imputations can be
done prior to passing price relatives to this function. (Missing values
still need to be removed in this example because not all missing prices
can be imputed.)

``` r
library(piar)

elementals <- ms_prices |>
  transform(
    imputed_price = carry_forward(price, period = period, product = product)
  ) |>
  elementary_index(
    price_relative(imputed_price, period = period, product = product) ~
      period + business,
    na.rm = TRUE
  )

elementals
```

    ## Period-over-period price index for 4 levels over 4 time periods 
    ##       time
    ## levels 202001    202002    202003   202004
    ##     B1      1 0.8949097 0.5781816 1.000000
    ##     B2      1 1.0000000 0.1777227 2.770456
    ##     B3      1 2.0200036 1.6353355 0.537996
    ##     B4    NaN       NaN       NaN 4.576286

## Non-parental imputation during aggregation

Parental imputation is the usual way to impute missing elementary index
values during aggregation, and it is simple to do with
[`aggregate()`](https://rdrr.io/r/stats/aggregate.html). In some cases,
however, an elementary index may get imputed with the value for, say,
another elementary aggregate, rather than for an entire group of
elementary aggregates. The simplest way to do this sort of imputation is
to alter the elementary indexes prior to aggregation.

As an example, suppose that missing index values for business B4 should
be imputed as 1, rather than the value for group 12. This replacement
can be done as if the index was a matrix.

``` r
elementals["B4", 1:3] <- 1

elementals
```

    ## Period-over-period price index for 4 levels over 4 time periods 
    ##       time
    ## levels 202001    202002    202003   202004
    ##     B1      1 0.8949097 0.5781816 1.000000
    ##     B2      1 1.0000000 0.1777227 2.770456
    ##     B3      1 2.0200036 1.6353355 0.537996
    ##     B4      1 1.0000000 1.0000000 4.576286
