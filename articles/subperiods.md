# Aggregating over subperiods

With the exception of
[`vignette("spatial-price-index")`](https://marberts.github.io/piar/articles/spatial-price-index.md),
all the examples so far have revolved around aggregating over the levels
of a price index for each time period. Although this is the core
workflow in this package, it is also useful to be able to aggregate over
the time periods for each level of an index to turn a monthly or
quarterly index into an annual index.

Let’s modify the example in
[`vignette("adjust-weights")`](https://marberts.github.io/piar/articles/adjust-weights.md)
by adding an additional eight quarters to make three years of data.

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
# Make elementary indexes over 3 years and aggregate.
quarterly_index <- matrix(
  runif(12 * 12, 0.4, 1.2),
  nrow = 12,
  dimnames = list(sprintf("B%02d", 1:12), paste0("Q", 1:12))
) |>
  as_index() |>
  aggregate(pias)

head(quarterly_index)
```

    ## Period-over-period price index for 6 levels over 12 time periods 
    ##       time
    ## levels        Q1        Q2        Q3        Q4        Q5        Q6        Q7
    ##    1   0.6847172 0.7513116 0.8602744 0.9150072 0.6691529 0.7621597 0.7928512
    ##    11  0.6442816 0.9426238 0.9800151 0.9787698 0.9366504 0.8698449 0.8068903
    ##    12  0.6553744 0.8448298 0.7877433 0.7364719 0.6864661 0.6092876 0.5790867
    ##    13  0.7125093 0.6568730 0.8763973 1.0105002 0.5713334 0.7912029 0.8792723
    ##    111 0.7802316 0.7678844 0.8888722 1.0294469 0.9856799 1.0533765 1.1007037
    ##    112 0.5860173 1.0423311 1.0183284 0.9601751 0.9173623 0.7922671 0.6417663
    ##       time
    ## levels        Q8        Q9       Q10       Q11       Q12
    ##    1   0.9285013 0.7802293 0.8192686 0.7150868 0.9343166
    ##    11  1.0502027 0.8297739 0.9545600 0.7823591 0.8891911
    ##    12  0.7510670 1.0351129 0.6936704 0.5073677 0.7432221
    ##    13  0.9130143 0.6873897 0.7607797 0.7188072 1.0262155
    ##    111 1.1442371 0.7836807 1.0508585 0.5157759 1.1069026
    ##    112 0.9595630 0.8827540 0.8562962 1.1161902 0.7632120

The conventional way to turn a quarterly arithmetic index into an annual
one is to take the (unweighted) arithmetic mean of the index values over
each year and rebase to a new base year.

``` r
annual_index <- chain(quarterly_index) |>
  mean(window = 4)

annual_index |>
  rebase(base = "Q1") |>
  head()
```

    ## Fixed-base price index for 6 levels over 3 time periods 
    ##       time
    ## levels Q1        Q5         Q9
    ##    1    1 0.3875906 0.17112143
    ##    11   1 0.7431798 0.46232765
    ##    12   1 0.2497373 0.07096693
    ##    13   1 0.3687070 0.14792335
    ##    111  1 0.9971250 0.72915914
    ##    112  1 0.6323628 0.34588738

It’s worth noting that, at least with an arithmetic index, the
aggregation properties of the index continue to hold after
price-updating the weights.

``` r
annual_pias <- pias |>
  update(annual_index, period = "Q1")

annual_index <- annual_index |>
  rebase(base = "Q1")

annual_index |>
  aggregate(annual_pias) |>
  all.equal(annual_index)
```

    ## [1] TRUE

This means that, for example, the workflow in
[`vignette("contributions")`](https://marberts.github.io/piar/articles/contributions.md)
for determining, say, the quarter-over-quarter contribution of the level
2 indexes to top-level index remains the same.

``` r
annual_index |>
  unchain() |>
  set_contrib_from_index() |>
  aggregate(cut(annual_pias, 2)) |>
  contrib()
```

    ##        time
    ## product Q1          Q5         Q9
    ##      11  0 -0.03908184 -0.1102682
    ##      12  0 -0.24028499 -0.1477187
    ##      13  0 -0.33304262 -0.3005126
