# Index-number formulas

Price indexes based on a generalized mean of price relatives constitute
a large family of bilateral index-number formulas that are consistent in
aggregation, and any index based on the generalized mean can be used to
make and aggregate elementary indexes. To see how to make superlative
indexes as well, let’s start with a simple dataset of prices and
quantities for two businesses over three periods. We’ll be making
extensive use of the *gpindex* package for lower-level price-index
functions; see the help pages for that package to get more detail.

``` r
library(piar)
library(gpindex)

prices <- data.frame(
  period = rep(1:3, each = 6),
  product = paste0("P", 1:6),
  business = rep(c("B1", "B2"), each = 3),
  price = 1:18,
  quantity = 18:1
)

prices[c("back_price", "back_quantity")] <-
  prices[back_period(prices$period, prices$product), c("price", "quantity")]

head(prices)
```

    ##   period product business price quantity back_price back_quantity
    ## 1      1      P1       B1     1       18          1            18
    ## 2      1      P2       B1     2       17          2            17
    ## 3      1      P3       B1     3       16          3            16
    ## 4      1      P4       B2     4       15          4            15
    ## 5      1      P5       B2     5       14          5            14
    ## 6      1      P6       B2     6       13          6            13

## Basic indexes

As seen in
[`vignette("piar")`](https://marberts.github.io/piar/articles/piar.md),
by default the
[`elementary_index()`](https://marberts.github.io/piar/reference/elementary_index.md)
function calculates a Jevons index (equally-weighted geometric mean of
price relatives). Although this is the standard index-number formula for
making elementary indexes, many other types of index-numbers are
possible. Among the unweighted index-number formulas, the Carli index
(equally-weighted arithmetic mean of price relatives) is the historical
competitor to the Jevons, and requires specifying the order of the
generalized mean `r` when calling
[`elementary_index()`](https://marberts.github.io/piar/reference/elementary_index.md).
An order of 1 corresponds to an arithmetic mean.

``` r
prices |>
  elementary_index(price / back_price ~ period + business, r = 1)
```

    ## Period-over-period price index for 2 levels over 3 time periods 
    ##       time
    ## levels 1        2        3
    ##     B1 1 4.666667 1.757937
    ##     B2 1 2.233333 1.548485

The Coggeshall index (equally-weighted harmonic mean of price relatives)
is another competitor to the Jevons, but is seldom used in practice.
Despite it being more exotic, it is just as easy to make by specifying
an order `r` of -1.

``` r
prices |>
  elementary_index(price / back_price ~ period + business, r = -1)
```

    ## Period-over-period price index for 2 levels over 3 time periods 
    ##       time
    ## levels 1        2        3
    ##     B1 1 4.131148 1.754499
    ##     B2 1 2.214765 1.547408

Weights can be added to make, for example, elementary indexes using the
geometric Laspeyres formula.

``` r
prices |>
  elementary_index(
    price / back_price ~ period + business,
    weights = back_price * back_quantity
  )
```

    ## Period-over-period price index for 2 levels over 3 time periods 
    ##       time
    ## levels 1        2        3
    ##     B1 1 3.853330 1.754015
    ##     B2 1 2.202496 1.549081

The type of mean used to aggregate elementary indexes can be controlled
in the same way in the call to
[`aggregate()`](https://rdrr.io/r/stats/aggregate.html). The default
makes an arithmetic index, but any type of generalized-mean index is
possible.

## Superlative indexes

Many superlative indexes can be made by supplying unequal and
time-varying weights when making the elementary indexes, usually from
information about quantities. The Törnqvist index is a popular
superlative index-number formula, using average period-over-period value
shares as the weights in a geometric mean. As
[`elementary_index()`](https://marberts.github.io/piar/reference/elementary_index.md)
makes a geometric index by default, all that is needed to make a
Törnqvist index is the weights.

``` r
tw <- grouped(index_weights("Tornqvist"))

prices |>
  elementary_index(
    price / back_price ~ period + business,
    weights = tw(
      price,
      back_price,
      quantity,
      back_quantity,
      group = interaction(period, business)
    )
  )
```

    ## Period-over-period price index for 2 levels over 3 time periods 
    ##       time
    ## levels 1        2        3
    ##     B1 1 4.087422 1.759213
    ##     B2 1 2.215995 1.556014

Making a Fisher index is more complex because it does not belong to the
generalized-mean family. Despite this, it is possible to make weights to
represent a Fisher index as a generalized mean of any order.

``` r
fw <- grouped(nested_transmute(0, c(1, -1), 0))

prices |>
  elementary_index(
    price / back_price ~ period + business,
    weights = fw(
      price / back_price,
      back_price * back_quantity,
      price * quantity,
      group = interaction(period, business)
    )
  )
```

    ## Period-over-period price index for 2 levels over 3 time periods 
    ##       time
    ## levels 1        2        3
    ##     B1 1 4.076840 1.759215
    ##     B2 1 2.215934 1.556046

Aggregating with a superlative index is more complex, and is the subject
of
[`vignette("superlative-aggregation")`](https://marberts.github.io/piar/articles/superlative-aggregation.md).

## Product contributions

As shown in
[`vignette("contributions")`](https://marberts.github.io/piar/articles/contributions.md),
supplying `contrib = TRUE` in the call to
[`elementary_index()`](https://marberts.github.io/piar/reference/elementary_index.md)
makes percent-change product contributions for each index value. The
method used in this package is flexible and works well for a variety of
different index-number formulas, but does not include all methods found
in the literature. (See the help page for
[`elementary_index()`](https://marberts.github.io/piar/reference/elementary_index.md),
and the references therein, for more detail about the exact methods.)

Let’s extend the previous example by calculating product contributions
for the Fisher index using the default method.

``` r
fisher_index <- prices |>
  elementary_index(
    price / back_price ~ period + business,
    weights = fw(
      price / back_price,
      back_price * back_quantity,
      price * quantity,
      group = interaction(period, business)
    ),
    contrib = TRUE
  )

contrib(fisher_index, "B1")
```

    ##        time
    ## product 1         2         3
    ##    B1.1 0 1.1024526 0.2899319
    ##    B1.2 0 1.0256151 0.2530718
    ##    B1.3 0 0.9487724 0.2162114

We can change the method used to make contributions for, say, business
`B1` by making a function to compute contributions according to a
different method

``` r
diewert_contributions <- function(p1, p0, q1, q0) {
  Pf <- fisher_index(p1, p0, q1, q0)
  Pl <- laspeyres_index(p1, p0, q0)
  wl <- scale_weights(index_weights("Laspeyres")(p0, q0))
  wp <- scale_weights(index_weights("HybridPaasche")(p0, q1))

  (1 / (1 + Pf) * wl + Pl / (1 + Pf) * wp) * (p1 / p0 - 1)
}
```

and using this function to replace the contributions for business `B1`.

``` r
contrib(fisher_index, "B1") <- subset(prices, business == "B1") |>
  split(~period) |>
  sapply(
    \(df) {
      diewert_contributions(
        df$price,
        df$back_price,
        df$quantity,
        df$back_quantity
      )
    }
  )

contrib(fisher_index, "B1")
```

    ##        time
    ## product 1         2         3
    ##       1 0 1.1124046 0.2937263
    ##       2 0 1.0256134 0.2530717
    ##       3 0 0.9388222 0.2124170

Aggregating the elementary indexes will then consistently aggregate the
contributions for both businesses, even though they use different
methods.
