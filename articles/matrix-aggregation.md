# Matrix aggregation

Aggregating a price index can be done as a matrix operation. Although
this approach is less flexible than the
[`aggregate()`](https://rdrr.io/r/stats/aggregate.html) method used in
[`vignette("piar")`](https://marberts.github.io/piar/articles/piar.md)
(i.e., there can be no missing elementary indexes), it can be
considerably faster for larger indexes.

Let’s start by building the index in
[`vignette("piar")`](https://marberts.github.io/piar/articles/piar.md)
again.

``` r
library(piar)

# Make an aggregation structure.
ms_weights[c("level1", "level2")] <-
  expand_classification(ms_weights$classification)

pias <- ms_weights[c("level1", "level2", "business", "weight")] |>
  as_aggregation_structure()

# Make a fixed-base index.
elementals <- ms_prices |>
  transform(
    relative = price_relative(price, period = period, product = product),
    business = factor(business, levels = ms_weights$business)
  ) |>
  elementary_index(relative ~ period + business, na.rm = TRUE)

index <- elementals |>
  aggregate(pias, na.rm = TRUE) |>
  chain()

index
```

    ## Fixed-base price index for 8 levels over 4 time periods 
    ##       time
    ## levels 202001    202002    202003    202004
    ##     1       1 1.3007239 1.3827662 3.7815355
    ##     11      1 1.3007239 1.3827662 2.1771866
    ##     12      1 1.3007239 1.3827662 6.3279338
    ##     B1      1 0.8949097 0.2991629 0.4710366
    ##     B2      1 1.3007239 1.3827662 3.8308934
    ##     B3      1 2.0200036 3.3033836 1.7772072
    ##     B4      1 1.3007239 1.3827662 6.3279338
    ##     B5      1 1.3007239 1.3827662 6.3279338

The key to do this aggregation as a matrix operation is to first turn
the aggregation structure into an aggregation matrix.

``` r
pias_matrix <- as.matrix(pias)

pias_matrix
```

    ##       
    ## levels        B1        B2        B3        B4        B5
    ##     1  0.2245229 0.2622818 0.1266748 0.2525376 0.1339829
    ##     11 0.3659828 0.4275314 0.2064858 0.0000000 0.0000000
    ##     12 0.0000000 0.0000000 0.0000000 0.6533613 0.3466387

Multiplying this matrix with a matrix of fixed-base elementary indexes
now computes the aggregate index in each time period.

``` r
pias_matrix %*% as.matrix(index[levels(pias)$business])
```

    ##       time
    ## levels 202001   202002   202003   202004
    ##     1       1 1.300724 1.382766 3.781536
    ##     11      1 1.300724 1.382766 2.177187
    ##     12      1 1.300724 1.382766 6.327934

## Computing the shadow of an index

It’s often useful to determine which higher-level index values are
missing, and subsequently get imputed during aggregation (i.e., compute
the shadow of an index). This is simple to do if there’s an elementary
index for each elementary aggregate in the aggregation structure.

The idea is to aggregate an indicator for missingness to get a matrix
that gives the share of missing elementary indexes for each higher-level
index.

``` r
pias_matrix <- as.matrix(pias) > 0

pias_matrix %*% is.na(elementals) / rowSums(pias_matrix)
```

    ##       time
    ## levels 202001    202002    202003    202004
    ##     1     0.4 0.6000000 0.6000000 0.4000000
    ##     11    0.0 0.3333333 0.3333333 0.3333333
    ##     12    1.0 1.0000000 1.0000000 0.5000000

A value of 1 means that there are no non-missing elementary indexes, and
that the value for this level of the index is imputed from its parent in
the aggregation structure. A value below 1 but above zero means that
some but not all elementary indexes are missing, and the index value for
this level is based on the non-missing elementary indexes. A value of
zero means there’s no imputation for this level of the index.

## Sparse matrices

Aggregation structures are naturally sparse. Although using a dense
aggregation matrix does not matter for small indexes, it quickly becomes
inefficient for large indexes—in this case it is better to make a sparse
aggregation matrix.

``` r
as.matrix(pias, sparse = TRUE)
```

    ## 3 x 5 sparse Matrix of class "dgCMatrix"
    ##       
    ## levels        B1        B2        B3        B4        B5
    ##     1  0.2245229 0.2622818 0.1266748 0.2525376 0.1339829
    ##     11 0.3659828 0.4275314 0.2064858 .         .        
    ##     12 .         .         .         0.6533613 0.3466387
