# Adjusting annual weights

Making a monthly or quarterly Lowe index with annual expenditure or
revenue weights requires adjusting these weights so that the implicit
annual quantity vector is used as the fixed basket when making the index
with the usual two-step procedure. This can be done by price updating
the annual weights to the base period of the index, and it serves as a
good example of extending the functions in this package.

Let’s start by making some annual weights and quarterly indexes for a
year.

``` r
set.seed(54321)

library(piar)

# Make an aggregation structure.
#                  1
#      |-----------|-----------|
#      11          12          13
#  |---+---|   |---+---|   |---+---|
# 111     121 121     122 131     132

pias <- data.frame(
  level1 = rep(1, 12),
  level2 = rep(c(11, 12, 13), each = 4),
  level3 = rep(c(111, 112, 121, 122, 131, 132), each = 2),
  ea = sprintf("B%02d", 1:12),
  weight = 1:12
) |>
  as_aggregation_structure()

pias
```

    ## Aggregation structure for 12 elementary aggregates with 3 levels above the elementary aggregates 
    ##    level1 level2 level3  ea weight
    ## 1       1     11    111 B01      1
    ## 2       1     11    111 B02      2
    ## 3       1     11    112 B03      3
    ## 4       1     11    112 B04      4
    ## 5       1     12    121 B05      5
    ## 6       1     12    121 B06      6
    ## 7       1     12    122 B07      7
    ## 8       1     12    122 B08      8
    ## 9       1     13    131 B09      9
    ## 10      1     13    131 B10     10
    ## 11      1     13    132 B11     11
    ## 12      1     13    132 B12     12

``` r
# Make elementary indexes over 4 quarters.
elementals <- matrix(
  runif(12 * 4, 0.4, 1.2),
  nrow = 12,
  dimnames = list(sprintf("B%02d", 1:12), paste0("Q", 1:4))
) |>
  as_index()

elementals
```

    ## Period-over-period price index for 12 levels over 4 time periods 
    ##       time
    ## levels        Q1        Q2        Q3        Q4
    ##    B01 0.7432063 0.4362399 1.1441564 1.1277327
    ##    B02 0.7987443 0.9221768 0.8326890 0.9997250
    ##    B03 0.5413539 1.1952528 0.9594449 1.0966120
    ##    B04 0.6195148 0.9421099 1.0672886 0.8581942
    ##    B05 0.5732081 1.1348361 0.4098346 1.1227607
    ##    B06 1.0930889 0.7699560 1.1682339 0.5616191
    ##    B07 0.4395281 0.8571318 0.9445658 0.9494140
    ##    B08 0.5673079 0.7615511 0.4677992 0.7279183
    ##    B09 0.6732899 0.5341656 1.1756704 1.1541544
    ##    B10 0.6973270 0.4546091 0.4928318 0.8646791
    ##    B11 0.5093139 1.1175286 1.1014889 0.8980850
    ##    B12 0.9408381 0.6190696 0.7101399 1.1024539

Adjusting the weights is simple when there are no missing elementary
indexes: the weight for each elementary aggregate is just divided by the
average (fixed-base) index for each quarter.

``` r
weights(pias) / rowMeans(as.matrix(chain(elementals)))
```

    ##       B01       B02       B03       B04       B05       B06       B07       B08 
    ##  2.154344  2.896610  4.819251  6.777709 11.175522  6.916156 18.543541 23.728960 
    ##       B09       B10       B11       B12 
    ## 18.520664 30.635776 19.396354 20.059400

The procedure is more complicated with missing elementary indexes as
reaggregating these indexes with the newly adjusted weights will
generally result in different imputations for the missing elementary
indexes, which in turn gives a different adjustment for the weights. In
practice this procedure is done a few times until the index values
converge to a fixed point. The following function shows how to do this
adjustment using the tools in this package.

``` r
# Function to adjust annual weights.
adjust_weights <- function(
  index,
  pias,
  tol = .Machine$double.eps^0.5,
  max_iter = 100
) {
  adj_pias <- pias
  for (i in seq_len(max_iter)) {
    # Parentally impute missing elementary indexes.
    agg_index <- aggregate(index, adj_pias, na.rm = TRUE, contrib = FALSE)
    elementals <- chain(agg_index[levels(pias)[[nlevels(pias)]]])
    # Compute annual elementary indexes.
    pb <- rowMeans(as.matrix(elementals))
    # Stop if average price-update weights are within tolerance of original
    # weights; adjust otherwise.
    if (max(abs(pb * weights(adj_pias) - weights(pias))) < tol) {
      message(gettextf("Converged after %d iterations", i - 1))
      return(adj_pias)
    } else {
      weights(adj_pias) <- weights(pias) / pb
    }
  }
  warning("weights adjustment did not converge")
  adj_pias
}
```

``` r
elementals[11:12] <- NA

adjust_weights(elementals, pias)
```

    ## Converged after 2 iterations

    ## Aggregation structure for 12 elementary aggregates with 3 levels above the elementary aggregates 
    ##    level1 level2 level3  ea    weight
    ## 1       1     11    111 B01  2.154344
    ## 2       1     11    111 B02  2.896610
    ## 3       1     11    112 B03  4.819251
    ## 4       1     11    112 B04  6.777709
    ## 5       1     12    121 B05 11.175522
    ## 6       1     12    121 B06  6.916156
    ## 7       1     12    122 B07 18.543541
    ## 8       1     12    122 B08 23.728960
    ## 9       1     13    131 B09 18.520664
    ## 10      1     13    131 B10 30.635776
    ## 11      1     13    132 B11 28.458991
    ## 12      1     13    132 B12 31.046172
