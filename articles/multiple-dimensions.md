# Aggregating across dimensions

Price indexes are often aggregated over multiple dimensions. Matched
sample indexes that use sequential Poisson sampling to draw a sample of
businesses are a good example, as there are usually take-all and
take-some strata in addition to, say, an industry classification.

Let’s extend
[`vignette("piar")`](https://marberts.github.io/piar/articles/piar.md)
by adding another dimension to the classification to say if a business
belongs to the take-all or take-some sampling stratum.

``` r
library(piar)

elementals <- ms_prices |>
  transform(
    relative = price_relative(price, period = period, product = product)
  ) |>
  elementary_index(relative ~ period + business, na.rm = TRUE)
```

``` r
ms_weights$stratum <- c("TS", "TA", "TS", "TS", "TS")

ms_weights
```

    ##   business classification weight stratum
    ## 1       B1             11    553      TS
    ## 2       B2             11    646      TA
    ## 3       B3             11    312      TS
    ## 4       B4             12    622      TS
    ## 5       B5             12    330      TS

The easiest way to deal with multiple digit-wise classifications is to
concatenate them into one classification. In this example the “stratum”
dimension comes before the “classification” dimension for the purposes
of parental imputation. This classification can be expanded with the
[`expand_classification()`](https://marberts.github.io/piar/reference/expand_classification.md)
function as before, just with an extra instruction to say that the last
“digit” in the classification is two characters wide, not one.

``` r
classification_sps <- paste0(ms_weights$classification, ms_weights$stratum) |>
  expand_classification(width = c(1, 1, 2))

pias_sps <- aggregation_structure(
  c(classification_sps, list(ms_weights$business)),
  ms_weights$weight
)

pias_sps
```

    ## Aggregation structure for 5 elementary aggregates with 3 levels above the elementary aggregates 
    ##   level1 level2 level3 ea weight
    ## 1      1     11   11TS B1    553
    ## 2      1     11   11TA B2    646
    ## 3      1     11   11TS B3    312
    ## 4      1     12   12TS B4    622
    ## 5      1     12   12TS B5    330

The elementary indexes can now be aggregated according to this new
aggregation structure.

``` r
index_sps <- aggregate(elementals, pias_sps, na.rm = TRUE)

index_sps
```

    ## Period-over-period price index for 11 levels over 4 time periods 
    ##       time
    ## levels 202001    202002    202003   202004
    ##   1         1 1.3007239 1.0630743 2.684412
    ##   11        1 1.3007239 1.0630743 1.492443
    ##   12        1 1.3007239 1.0630743 4.576286
    ##   11TS      1 1.3007239 1.0630743 0.537996
    ##   11TA      1 1.3007239 1.0630743 2.770456
    ##   12TS      1 1.3007239 1.0630743 4.576286
    ##   B1        1 0.8949097 0.3342939 0.537996
    ##   B2        1 1.3007239 1.0630743 2.770456
    ##   B3        1 2.0200036 1.6353355 0.537996
    ##   B4        1 1.3007239 1.0630743 4.576286
    ##   B5        1 1.3007239 1.0630743 4.576286

When a price index has many dimensions (e.g., industry, sampling
stratum, region), it can be useful to interact the classifications for
these different dimensions to get all possible aggregation structures.
The aggregated index can then be re-aggregated to get index values for
all dimensions.

Continuing with the example, the industry and strata classifications can
be interacted to get two aggregation structures that can be used to
re-aggregate `index_sps`.

``` r
interacted_hierarchy <- interact_classifications(
  expand_classification(ms_weights$classification),
  expand_classification(ms_weights$stratum)
)

pias_sps2 <- lapply(
  interacted_hierarchy,
  \(x) aggregation_structure(c(x, list(ms_weights$business)), ms_weights$weight)
)

index_sps2 <- lapply(pias_sps2, \(x) {
  aggregate(index_sps, x, include_ea = FALSE)
})
```

The resulting indexes can be merged together to give an index that
includes all combinations of industry and sampling stratum.

``` r
Reduce(merge, index_sps2)
```

    ## Period-over-period price index for 8 levels over 4 time periods 
    ##        time
    ## levels  202001   202002   202003   202004
    ##   1:T        1 1.300724 1.063074 2.684412
    ##   1:TS       1 1.300724 1.063074 2.653820
    ##   1:TA       1 1.300724 1.063074 2.770456
    ##   11:T       1 1.300724 1.063074 1.492443
    ##   12:T       1 1.300724 1.063074 4.576286
    ##   11:TS      1 1.300724 1.063074 0.537996
    ##   11:TA      1 1.300724 1.063074 2.770456
    ##   12:TS      1 1.300724 1.063074 4.576286
