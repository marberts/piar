# Making price indexes

Most price indexes are made with a two-step procedure, where
period-over-period *elementary indexes* are calculated for a collection
of *elementary aggregates* at each point in time, and then aggregated
according to a *price index aggregation structure*. These indexes can
then be chained together to form a time series that gives the evolution
of prices with respect to a fixed base period. This package contains a
collection of functions that revolve around this work flow, making it
easy to build standard price indexes in **R**.

The purpose of this vignette is to give an introductory example for how
to use the core functionality in this package to make a standard price
index. Subsequent vignettes go into more details on advanced topics,
often referencing the example in this vignette.

## Matched-sample index

In this vignette we’ll be calculating a matched-sample index where a
fixed set of businesses each provide prices for a collection of products
over time. The products reported by a businesses can change over time,
but the set of businesses is fixed for the duration of the sample. Each
businesses has a weight that is established when the sample is drawn and
represents a particular segment of the economy.

The usual approach for calculating a matched-sample index starts by
computing an elementary index for each business as an equally-weighted
geometric mean of price relatives (i.e., a Jevons index). From there,
index values for different segments of the economy are calculated as an
arithmetic mean of the elementary indexes using the businesses-level
weights (either a Young or Lowe index, depending how the weights are
constructed; see
[`vignette("adjust-weights")`](https://marberts.github.io/piar/articles/adjust-weights.md)).

The `ms_prices` dataset has price data for five businesses over four
quarters, and the `ms_weights` dataset has the weight data. Note that
these data have fairly realistic patterns of missing data and are
emblematic of the kinds of survey data used to make price indexes.

``` r
library(piar)

head(ms_prices)
```

    ##   period business product price
    ## 1 202001       B1       1  1.14
    ## 2 202001       B1       2    NA
    ## 3 202001       B1       3  6.09
    ## 4 202001       B2       4  6.23
    ## 5 202001       B2       5  8.61
    ## 6 202001       B2       6  6.40

``` r
ms_weights
```

    ##   business classification weight
    ## 1       B1             11    553
    ## 2       B2             11    646
    ## 3       B3             11    312
    ## 4       B4             12    622
    ## 5       B5             12    330

The
[`elementary_index()`](https://marberts.github.io/piar/reference/elementary_index.md)
function makes, well, elementary indexes, using information on price
relatives, elementary aggregates (businesses), and time periods
(quarters). By default it makes a Jevons index, but any bilateral
generalized-mean index is possible (see
[`vignette("index-number-formulas")`](https://marberts.github.io/piar/articles/index-number-formulas.md)
for more details). The only wrinkle is that price data here are in
levels, and not relatives, but the
[`price_relative()`](https://marberts.github.io/piar/reference/price_relative.md)
function can make the necessary conversion.

``` r
elementals <- ms_prices |>
  transform(
    relative = price_relative(price, period = period, product = product)
  ) |>
  elementary_index(relative ~ period + business, na.rm = TRUE)

elementals
```

    ## Period-over-period price index for 4 levels over 4 time periods 
    ##       time
    ## levels 202001    202002    202003   202004
    ##     B1      1 0.8949097 0.3342939      NaN
    ##     B2      1       NaN       NaN 2.770456
    ##     B3      1 2.0200036 1.6353355 0.537996
    ##     B4    NaN       NaN       NaN 4.576286

As with most functions in **R**, missing values are contagious by
default. Setting `na.rm = TRUE` in
[`elementary_index()`](https://marberts.github.io/piar/reference/elementary_index.md)
means that missing price relatives are ignored, which is equivalent to
imputing these missing relatives with the value of the elementary index
for the respective businesses (i.e., parental or overall mean
imputation). Other types of imputation are covered in
[`vignette("imputation")`](https://marberts.github.io/piar/articles/imputation.md).

The
[`elementary_index()`](https://marberts.github.io/piar/reference/elementary_index.md)
function returns a special index object, and there are a number of
methods for working with these objects. For example, the resulting
indexes to be extracted like a matrix even though it’s not a
matrix.[¹](#fn1)

``` r
elementals[, "202004"]
```

    ## Period-over-period price index for 4 levels over 1 time periods 
    ##       time
    ## levels   202004
    ##     B1      NaN
    ##     B2 2.770456
    ##     B3 0.537996
    ##     B4 4.576286

``` r
elementals[c("B1", "B3"), ]
```

    ## Period-over-period price index for 2 levels over 4 time periods 
    ##       time
    ## levels 202001    202002    202003   202004
    ##     B1      1 0.8949097 0.3342939      NaN
    ##     B3      1 2.0200036 1.6353355 0.537996

With the elementary indexes out of the way, it’s time to make a
price-index aggregation structure that maps each business to its
position in the aggregation hierarchy. The only hiccup is unpacking the
digit-wise classification for each businesses that defines the
hierarchy. That’s the job of the
[`expand_classification()`](https://marberts.github.io/piar/reference/expand_classification.md)
function.

``` r
ms_weights[c("level1", "level2")] <-
  expand_classification(ms_weights$classification)

pias <- ms_weights[c("level1", "level2", "business", "weight")] |>
  as_aggregation_structure()
```

It is now simple to aggregate the elementary indexes according to this
aggregation structure with the
[`aggregate()`](https://rdrr.io/r/stats/aggregate.html) function. As
with the elementary indexes, missing values are ignored by setting
`na.rm = TRUE`, which is equivalent to parentally imputing missing
values. Note that, unlike the elementary indexes, missing values are
filled in to ensure the index can be chained over time.

``` r
index <- aggregate(elementals, pias, na.rm = TRUE)

index
```

    ## Period-over-period price index for 8 levels over 4 time periods 
    ##       time
    ## levels 202001    202002    202003   202004
    ##     1       1 1.3007239 1.0630743 2.734761
    ##     11      1 1.3007239 1.0630743 1.574515
    ##     12      1 1.3007239 1.0630743 4.576286
    ##     B1      1 0.8949097 0.3342939 1.574515
    ##     B2      1 1.3007239 1.0630743 2.770456
    ##     B3      1 2.0200036 1.6353355 0.537996
    ##     B4      1 1.3007239 1.0630743 4.576286
    ##     B5      1 1.3007239 1.0630743 4.576286

## Chaining

The
[`elementary_index()`](https://marberts.github.io/piar/reference/elementary_index.md)
function makes period-over-period elementary indexes by default, which
are then aggregated to make a period-over-period index. Chaining an
index is the process of taking the cumulative product of each of these
period-over-period indexes to make a time series that compares prices to
a fixed base period.

The [`chain()`](https://marberts.github.io/piar/reference/chain.md)
function can be used to chain the values in an index object.

``` r
chained_index <- chain(index)

chained_index
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

This gives almost the same result as directly manipulating the index as
a matrix, except that the former returns an index object (not a matrix).

Chained indexes often need be to rebased, and this can be done with the
[`rebase()`](https://marberts.github.io/piar/reference/chain.md)
function. For example, rebasing the index so that 202004 is the base
period just requires dividing the chained index by the slice for 202004.

``` r
rebase(chained_index, chained_index[, "202004"])
```

    ## Fixed-base price index for 8 levels over 4 time periods 
    ##       time
    ## levels    202001    202002    202003 202004
    ##     1  0.2644428 0.3439671 0.3656626      1
    ##     11 0.4593084 0.5974334 0.6351161      1
    ##     12 0.1580295 0.2055527 0.2185178      1
    ##     B1 2.1229774 1.8998731 0.6351161      1
    ##     B2 0.2610357 0.3395354 0.3609514      1
    ##     B3 0.5626806 1.1366169 1.8587499      1
    ##     B4 0.1580295 0.2055527 0.2185178      1
    ##     B5 0.1580295 0.2055527 0.2185178      1

## Working with indexes

Once an index has been calculated, it usually needs to be turned into a
table of index values. This can be done by either coercing an index into
a matrix

``` r
as.matrix(chained_index)
```

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

or a data frame

``` r
as.data.frame(chained_index)
```

    ##    period level     value
    ## 1  202001     1 1.0000000
    ## 2  202001    11 1.0000000
    ## 3  202001    12 1.0000000
    ## 4  202001    B1 1.0000000
    ## 5  202001    B2 1.0000000
    ## 6  202001    B3 1.0000000
    ## 7  202001    B4 1.0000000
    ## 8  202001    B5 1.0000000
    ## 9  202002     1 1.3007239
    ## 10 202002    11 1.3007239
    ## 11 202002    12 1.3007239
    ## 12 202002    B1 0.8949097
    ## 13 202002    B2 1.3007239
    ## 14 202002    B3 2.0200036
    ## 15 202002    B4 1.3007239
    ## 16 202002    B5 1.3007239
    ## 17 202003     1 1.3827662
    ## 18 202003    11 1.3827662
    ## 19 202003    12 1.3827662
    ## 20 202003    B1 0.2991629
    ## 21 202003    B2 1.3827662
    ## 22 202003    B3 3.3033836
    ## 23 202003    B4 1.3827662
    ## 24 202003    B5 1.3827662
    ## 25 202004     1 3.7815355
    ## 26 202004    11 2.1771866
    ## 27 202004    12 6.3279338
    ## 28 202004    B1 0.4710366
    ## 29 202004    B2 3.8308934
    ## 30 202004    B3 1.7772072
    ## 31 202004    B4 6.3279338
    ## 32 202004    B5 6.3279338

It is also sometimes useful to get the price-updated weights used to
aggregate the index; these can be calculated by first updating the
aggregation structure with the aggregated index, then made into a table.

``` r
update(pias, index) |>
  as.data.frame()
```

    ##   level1 level2 business    weight
    ## 1      1     11       B1  260.4832
    ## 2      1     11       B2 2474.7571
    ## 3      1     11       B3  554.4886
    ## 4      1     12       B4 3935.9748
    ## 5      1     12       B5 2088.2182

------------------------------------------------------------------------

1.  Note that there are only indexes for four businesses, not five,
    because the fifth business never reports any prices. An elementary
    index can be made for this business by passing a factor with a level
    for all five businesses to
    [`elementary_index()`](https://marberts.github.io/piar/reference/elementary_index.md).
