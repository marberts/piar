# Superlative aggregation

All of the examples so far have used a single set of weights to
aggregate an index. Although this is by far the most common case, it is
not suitable for aggregating a superlative index where there is more
than one set of aggregation weights that change every period.

## Aggregating a Paasche index

Let’s start by making a chained Paasche index from 6 elementary indexes
over 4 time periods to see how to deal with time-varying aggregation
weights. This is similar to the example of building an index across
multiple basket in
[`vignette("multiple-baskets")`](https://marberts.github.io/piar/articles/multiple-baskets.md),
except that the weights change each period.

``` r
library(piar)

set.seed(12345)

# Make 6 elementary indexes over 4 time periods.
elementals <- matrix(c(rep(1, 6), runif(6 * 3, 0.8, 1.2)), nrow = 6) |>
  as_index() |>
  set_levels(paste0("B", 1:6))

head(elementals)
```

    ## Period-over-period price index for 6 levels over 4 time periods 
    ##       time
    ## levels 1         2         3         4
    ##     B1 1 1.0883616 0.9300382 1.0942740
    ##     B2 1 1.1503093 1.0036897 0.8004546
    ##     B3 1 1.1043929 1.0910821 0.9564813
    ##     B4 1 1.1544498 1.1958948 0.9849979
    ##     B5 1 0.9825924 0.8138142 0.9552576
    ##     B6 1 0.8665487 0.8609494 0.9609941

``` r
# Make aggregation weights over 4 time periods.
#            1
#      |-----+-----|
#      11          12
#  |---+---|   |---+---|
#  B1  B2  B3  B4  B5  B6

weights <- data.frame(
  level1 = 1,
  level2 = rep(11:12, each = 3),
  ea = levels(elementals),
  weights = runif(4 * 6, 100, 200),
  period = rep(1:4, each = 6)
)

head(weights)
```

    ##   level1 level2 ea  weights period
    ## 1      1     11 B1 117.8964      1
    ## 2      1     11 B2 195.1659      1
    ## 3      1     11 B3 145.3728      1
    ## 4      1     12 B4 132.6752      1
    ## 5      1     12 B5 196.5415      1
    ## 6      1     12 B6 170.7482      1

The key tools to deal with time-varying aggregation weights are the
[`stack()`](https://rdrr.io/r/utils/stack.html) and
[`unstack()`](https://rdrr.io/r/utils/stack.html) functions.
[`stack()`](https://rdrr.io/r/utils/stack.html) appends a later index
series onto an earlier one for the same levels, whereas
[`unstack()`](https://rdrr.io/r/utils/stack.html) pulls apart an index
series for many periods into a collection of one-period
indexes.[¹](#fn1) These functions allow the aggregation to be done with
a map-reduce.

The first step to making a Paasche index is to unstack the elementary
indexes into a list of elementary indexes for each period. (Trying to
make the elementary indexes period-by-period can be dangerous when there
are missing values.)

``` r
elementals <- unstack(elementals)
```

This is followed by making a sequence of aggregation structures for each
set of weights.

``` r
paasche_pias <- split(
  weights[c("level1", "level2", "ea", "weights")],
  weights[["period"]]
) |>
  lapply(as_aggregation_structure)
```

Computing the Paasche index for each period is now just a case of
mapping the [`aggregate()`](https://rdrr.io/r/stats/aggregate.html)
function to each elementary index and aggregation structure, and then
reducing the result with the
[`stack()`](https://rdrr.io/r/utils/stack.html) function.

``` r
paasche <- Map(
  aggregate,
  elementals,
  paasche_pias,
  na.rm = TRUE,
  include_ea = FALSE,
  r = -1
) |>
  Reduce(stack, x = _)

paasche
```

    ## Period-over-period price index for 3 levels over 4 time periods 
    ##       time
    ## levels 1         2         3         4
    ##     1  1 1.0504784 0.9666398 0.9468934
    ##     11 1 1.1117341 0.9920294 0.9329698
    ##     12 1 0.9898158 0.9447976 0.9647535

## Making a Fisher index

Making a chained Fisher index requires two sets of weights: base-period
weights to make the Laspeyres index and current-period weights to make
the Paasche index. As with the Paasche index this can be done with a
map-reduce, except now by passing two aggregation structures to the
[`aggregate()`](https://rdrr.io/r/stats/aggregate.html) function instead
of one.

``` r
laspeyres_pias <- paasche_pias[c(1, 1, 2, 3)]

fisher <- Map(
  aggregate,
  elementals,
  pias = laspeyres_pias,
  pias2 = paasche_pias,
  na.rm = TRUE,
  include_ea = FALSE
) |>
  Reduce(stack, x = _)

fisher
```

    ## Period-over-period price index for 3 levels over 4 time periods 
    ##       time
    ## levels 1         2         3         4
    ##     1  1 1.0509134 0.9783795 0.9600792
    ##     11 1 1.1157686 1.0007074 0.9557479
    ##     12 1 0.9891911 0.9567760 0.9665552

This gives the same result as calculating the Laspeyres and Paasche
indexes individually, and then calculating the Fisher index manually.

``` r
laspeyres <- Map(
  aggregate,
  elementals,
  pias = laspeyres_pias,
  na.rm = TRUE,
  include_ea = FALSE
) |>
  Reduce(stack, x = _)

sqrt(as.matrix(laspeyres) * as.matrix(paasche))
```

    ##       time
    ## levels 1         2         3         4
    ##     1  1 1.0509134 0.9783795 0.9600792
    ##     11 1 1.1157686 1.0007074 0.9557479
    ##     12 1 0.9891911 0.9567760 0.9665552

------------------------------------------------------------------------

1.  [`split()`](https://rdrr.io/r/base/split.html) can also be used for
    this.
