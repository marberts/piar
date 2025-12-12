# Spatial price indexes

It is possible to rework the functions in this package to make a
spatial, rather than temporal, price index using a star model by simply
replacing “time” with “space”. Although this is not a suitable approach
for making a system of purchasing power parities, is it useful for
making indexes that adjust pay for cost-of-living differences relative
to a fixed location. These sorts of indexes are made by the United
Nations International Civil Service Commission, Eurostat, and various
governments and private-sector organizations for administering
cost-of-living allowances.

The basic workflow is the same as with a temporal index, just treating
each country as a different “time period” in a fixed-based index.

``` r
library(piar)

set.seed(12345)

# Make indexes for 6 basic headings for 4 countries.
bh_index <- matrix(
  c(rep(1, 6), runif(6 * 3, 0.8, 1.2)),
  nrow = 6,
  dimnames = list(
    paste0("BH", 1:6),
    paste("Country", 1:4)
  )
) |>
  as_index(chainable = FALSE)

head(bh_index)
```

    ## Fixed-base price index for 6 levels over 4 time periods 
    ##       time
    ## levels Country 1 Country 2 Country 3 Country 4
    ##    BH1         1 1.0883616 0.9300382 1.0942740
    ##    BH2         1 1.1503093 1.0036897 0.8004546
    ##    BH3         1 1.1043929 1.0910821 0.9564813
    ##    BH4         1 1.1544498 1.1958948 0.9849979
    ##    BH5         1 0.9825924 0.8138142 0.9552576
    ##    BH6         1 0.8665487 0.8609494 0.9609941

``` r
# Make fixed aggregation weights.
#            1
#      |-----+-----|
#      11          12
#  |---+---|   |---+---|
#  B1  B2  B3  B4  B5  B6

weights <- data.frame(
  level1 = 1,
  level2 = rep(11:12, each = 3),
  bh = levels(bh_index),
  weights = runif(6, 100, 200)
)

head(weights)
```

    ##   level1 level2  bh  weights
    ## 1      1     11 BH1 117.8964
    ## 2      1     11 BH2 195.1659
    ## 3      1     11 BH3 145.3728
    ## 4      1     12 BH4 132.6752
    ## 5      1     12 BH5 196.5415
    ## 6      1     12 BH6 170.7482

The indexes at the basic-heading level can be aggregated as usual to get
a collection of indexes that give the difference in purchasing power
relative to the base country (country 1 in this example). Different
choices for weights make different assumptions about how people change
their spending patterns between each country and the base country.

``` r
index <- aggregate(bh_index, weights)
```

As it can be costly to collect price data from various countries on a
regular basis, these sorts of indexes are often updated over time by
changes in exchange rates and inflation rates for each country.

``` r
as.matrix(index[1, -1])
```

    ##       time
    ## levels Country 2 Country 3 Country 4
    ##      1  1.051349 0.9701244 0.9461596

``` r
update_factors <- runif(3, 0.8, 1.2)

as.matrix(index[1, -1]) * update_factors
```

    ##       time
    ## levels Country 2 Country 3 Country 4
    ##      1  1.112134 0.9273724  1.021301
