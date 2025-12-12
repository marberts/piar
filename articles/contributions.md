# Contributions

## Product contributions

It’s often convenient to decompose an index into the (additive)
contribution of each price relative, also known as the percent-change
contribution. This can be done with the same work flow used in
[`vignette("piar")`](https://marberts.github.io/piar/articles/piar.md),
specifying `contrib = TRUE` when calling
[`elementary_index()`](https://marberts.github.io/piar/reference/elementary_index.md).

``` r
library(piar)

# Make an aggregation structure.
ms_weights[c("level1", "level2")] <-
  expand_classification(ms_weights$classification)

pias <- ms_weights[c("level1", "level2", "business", "weight")] |>
  as_aggregation_structure()

# Make elementary index with contributions.
elementals <- ms_prices |>
  transform(
    relative = price_relative(price, period = period, product = product)
  ) |>
  elementary_index(
    relative ~ period + business,
    product = product,
    na.rm = TRUE,
    contrib = TRUE
  )
```

As with index values, percent-change contributions for a given level of
the index can be extracted as a matrix.

``` r
contrib(elementals, level = "B1")
```

    ##        time
    ## product 202001     202002     202003 202004
    ##       1      0  0.0000000  0.0000000      0
    ##       2     NA         NA -0.6657061      0
    ##       3      0 -0.1050903         NA     NA

Or as a data frame.

``` r
contrib2DF(elementals, level = "B1")
```

    ##   period level product      value
    ## 1 202001    B1       1  0.0000000
    ## 2 202001    B1       2         NA
    ## 3 202001    B1       3  0.0000000
    ## 4 202002    B1       2         NA
    ## 5 202002    B1       3 -0.1050903
    ## 6 202003    B1       2 -0.6657061
    ## 7 202003    B1       3         NA
    ## 8 202004    B1       3         NA

Aggregating the elementary indexes automatically aggregates
percent-change contributions, so no extra steps are needed after the
elementary indexes are made.

``` r
index <- aggregate(elementals, pias, na.rm = TRUE)

contrib(index)
```

    ##        time
    ## product 202001      202002     202003       202004
    ##      1       0  0.00000000  0.0000000  0.000000000
    ##      10      0 -0.08782076  0.2731949 -0.078173579
    ##      11      0  0.00000000         NA  0.059392635
    ##      12      0  0.00000000         NA  1.322915301
    ##      2      NA          NA -0.2928098  0.000000000
    ##      3       0 -0.06718490         NA           NA
    ##      4       0          NA         NA -0.018209690
    ##      5       0          NA         NA  0.094562963
    ##      6       0          NA         NA  0.427935081
    ##      7       0  0.51646606 -0.2054665 -0.011177530
    ##      8       0  0.01906845  0.1755868 -0.003784845
    ##      9       0 -0.07980493  0.1125689 -0.058699008

## Index contributions

After an index has been calculated, it’s often useful to compute the
contribution of higher-level indexes towards the total index. The
easiest way to do this with a collection of pre-computed index values is
to simply coerce them into an index object with the index values as
contributions and reaggregate with a restricted aggregation structure.

``` r
index <- as_index(as.matrix(index), contrib = TRUE)
```

If the index values are already an index object, it’s also possible to
directly replace the contributions with the
[`set_contrib_from_index()`](https://marberts.github.io/piar/reference/contrib.md)
function. We can now cut the aggregation structure to keep only the top
two levels and reaggregate to get the contribution of the second-level
indexes to the top level index.

``` r
set_contrib_from_index(index) |>
  aggregate(cut(pias, 2)) |>
  contrib()
```

    ##        time
    ## product 202001   202002     202003    202004
    ##      11      0 0.184488 0.03869481 0.3524534
    ##      12      0 0.116236 0.02437952 1.3823079

The same approach works with a fixed-base index as well.

``` r
chain(index) |>
  set_contrib_from_index() |>
  aggregate(cut(pias, 2)) |>
  contrib()
```

    ##        time
    ## product 202001   202002    202003    202004
    ##      11      0 0.184488 0.2348192 0.7221798
    ##      12      0 0.116236 0.1479470 2.0593557
