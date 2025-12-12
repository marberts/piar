# Aggregating across baskets

Most prices indexes use the same set of weights to aggregate elementary
indexes for several years, periodically updating these weights and
adding or removing elementary aggregates. The general approach to keep
the time series going across baskets is to “chain” the index across
baskets.

Let’s start by making some quarterly elementary indexes and weights for
two baskets. Each basket has index values for two years, and there is a
one year overlap between the baskets.

``` r
library(piar)

set.seed(12345)

# Make quarterly elementary indexes for two baskets with a 1 year overlap.

elementals1 <- matrix(
  runif(5 * 8, 0.8, 1.2),
  nrow = 5,
  dimnames = list(paste0("B", 1:5), NULL)
) |>
  as_index()

elementals2 <- matrix(
  runif(6 * 8, 0.8, 1.2),
  nrow = 6,
  dimnames = list(paste0("B", 1:6), 5:12)
) |>
  as_index()

# Make aggregation weights for basket 1.
#            1
#      |-----+-----|
#      11          12
#  |---+---|   |---+---|
#  B1  B2  B3  B4      B5

weights1 <- data.frame(
  level1 = 1,
  level2 = c(11, 11, 11, 12, 12),
  ea = levels(elementals1),
  weights = runif(5, 100, 200)
)

# Make aggregation weights for basket 2.
#            1
#      |-----+-----|
#      11          12
#  |---+---|   |---+---|
#  B1  B2  B3  B4  B5  B6

weights2 <- data.frame(
  level1 = 1,
  level2 = c(11, 11, 11, 12, 12, 12),
  ea = levels(elementals2),
  weights = runif(6, 100, 200)
)
```

Aggregating the indexes for both baskets involves simply mapping the
usual workflow across both baskets, keeping only the higher-level
indexes because the elementary indexes change across baskets.

``` r
index <- Map(
  aggregate,
  list(elementals1, elementals2),
  list(weights1, weights2),
  include_ea = FALSE
)
```

There are now several ways to combine these index series to form a
single time series. Assuming that we don’t want to revise any index
values from the old basket, the easiest method is to simply ignore the
overlap period and stack the upper-level period-over-period indexes for
the new basket onto the those in the old basket.

``` r
stack(index[[1]], window(index[[2]], start = "9")) |>
  chain()
```

    ## Fixed-base price index for 3 levels over 12 time periods 
    ##       time
    ## levels        1        2         3         4        5        6         7
    ##     1  1.101645 1.109240 1.0073206 0.9850853 1.055811 1.037494 0.9931005
    ##     11 1.108522 1.037686 0.9807159 0.9482265 1.016769 1.028660 0.9505788
    ##     12 1.090230 1.228015 1.0514823 1.0462678 1.120616 1.052158 1.0636831
    ##       time
    ## levels        8        9        10        11        12
    ##     1  1.030081 1.082712 1.0297273 1.0833225 1.0117095
    ##     11 1.037308 1.149467 1.1530529 1.2475735 1.0303923
    ##     12 1.018084 1.024395 0.9280337 0.9496758 0.9909764

Alternatively, the index values from the new basket can be chained using
the values in the last period of the first basket as a link factor and
combined with the chained index values from the old basket.

``` r
link_factor <- chain(index[[1]]) |>
  window(start = end(index[[1]])) |>
  as.numeric()

stack(
  chain(index[[1]]),
  chain(window(index[[2]], start = "9"), link = link_factor)
)
```

    ## Fixed-base price index for 3 levels over 12 time periods 
    ##       time
    ## levels        1        2         3         4        5        6         7
    ##     1  1.101645 1.109240 1.0073206 0.9850853 1.055811 1.037494 0.9931005
    ##     11 1.108522 1.037686 0.9807159 0.9482265 1.016769 1.028660 0.9505788
    ##     12 1.090230 1.228015 1.0514823 1.0462678 1.120616 1.052158 1.0636831
    ##       time
    ## levels        8        9        10        11        12
    ##     1  1.030081 1.082712 1.0297273 1.0833225 1.0117095
    ##     11 1.037308 1.149467 1.1530529 1.2475735 1.0303923
    ##     12 1.018084 1.024395 0.9280337 0.9496758 0.9909764

The index values for the overlap period can be used to make more complex
link factors when the index series have an annual base period.

``` r
index <- index |>
  lapply(chain) |>
  lapply(\(x) rebase(x, mean(x[, 1:4])))
```

Keeping the base year of the old basket can be done by simply rebasing
the index series for the new basket.

``` r
link_factor <- window(index[[1]], start = "5") |>
  mean() |>
  as.numeric()

stack(
  index[[1]],
  rebase(window(index[[2]], start = "9"), base = 1 / link_factor)
)
```

    ## Fixed-base price index for 3 levels over 12 time periods 
    ##       time
    ## levels         1        2         3         4        5         6         7
    ##     1  1.0483642 1.055592 0.9586018 0.9374418 1.004747 0.9873156 0.9450694
    ##     11 1.0880796 1.018550 0.9626304 0.9307402 0.998019 1.0096899 0.9330491
    ##     12 0.9875283 1.112334 0.9524307 0.9477073 1.015052 0.9530428 0.9634821
    ##       time
    ## levels         8        9        10        11        12
    ##     1  0.9802611 1.036713 0.9859794 1.0372977 0.9687271
    ##     11 1.0181791 1.027851 1.0310566 1.1155766 0.9213738
    ##     12 0.9221784 1.037244 0.9396738 0.9615874 1.0034060

Updating the reference year to that of the new basket involves rebasing
the series for both the old basket and the new basket.

``` r
index[[1]] <- rebase(index[[1]], mean(window(index[[1]], start = "5")))

link_factor <- as.numeric(index[[1]][, "8"]) / as.numeric(index[[2]][, "8"])

stack(
  index[[1]],
  rebase(window(index[[2]], start = "9"), base = 1 / link_factor)
)
```

    ## Fixed-base price index for 3 levels over 12 time periods 
    ##       time
    ## levels        1        2        3         4        5         6         7
    ##     1  1.070471 1.077852 0.978816 0.9572099 1.025934 1.0081354 0.9649983
    ##     11 1.099365 1.029114 0.972615 0.9403940 1.008371 1.0201626 0.9427269
    ##     12 1.025004 1.154545 0.988574 0.9836715 1.053572 0.9892094 1.0000448
    ##       time
    ## levels         8         9        10        11        12
    ##     1  1.0009321 1.0520739 1.0005887 1.0526672 0.9830807
    ##     11 1.0287398 1.1399727 1.1435285 1.2372683 1.0218811
    ##     12 0.9571738 0.9631074 0.8725112 0.8928585 0.9316881
