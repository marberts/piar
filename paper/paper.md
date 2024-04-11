---
title: 'piar: Price Index Aggregation in R'
tags:
  - R
  - economics
  - inflation
  - official statistics
authors:
  - name: Steve Martin
    orcid: 0000-0003-2544-9480
    affiliation: 1
affiliations:
 - name: Statistics Canada, Canada
   index: 1
date: 9 April 2024
bibliography: paper.bib
---

# Summary

The systematic inflation of prices for goods and services over time is a
ubiquitous feature of modern economies. A price index is the main empirical tool
to measure changes in prices over time, and consequently price indexes are a core
macro-economic statistic produced by national statistical agencies to measure
inflation. Most price indexes, especially those coming from by national statistical
agencies, are made with a two-step procedure, where period-over-period
indexes are calculated for a large collection of well-defined goods and services at
each point in time and then aggregated according to a hierarchical structure.
These period-over-period indexes can then be chained together to form a time
series that gives the evolution of prices with respect to a fixed point in time.

`piar` is an R [@R] package for aggregating price indexes. It contains a
collection of functions that revolve around the usual two-step work flow for
computing price indexes, making it easy to build standard price indexes or research
new ones, and implement the methods described in the literature [e.g., @balk2008]
and the authoritative Consumer Price Index and Producer Price Index manuals [@cpimanual; @ppimanual]. `piar` is currently
used to produce a number of price indexes at Statistics Canada.

# Statement of need

`piar` fills a gap in the open-source ecosystem for measuring inflation by providing a tool for building the kinds of large,
hierarchical price indexes made by national statistical agencies. There are several R and Python package for accessing published price indexes [e.g., @vonbergmann2021; @welsh2024], but these do not enable computing new indexes. To compute a price index, there are a number of R packages that implement the textbook index-number formulas to measure the change in price over time for a collection of goods and services [e.g., @henningsen2022; @saavedra2021; @white2023]. These methods, however, are seldom directly used in practice; for example, textbook index-number formulas use information on both
prices and quantities over time, but quantity information is rarely available in most cases. Even when these methods are directly applicable, they apply to a single collection of goods and services at a point in time, whereas price indexes usually consider many collections of goods and services that are aggregated with a hierarchical structure over time. In contrast to existing tools, `piar` provides a flexible API to easily build large, hierarchical price indexes over time in a way that is suitable for both the data and methods used to make large-scale measures of inflation.

# Usage

The built-in `ms_prices` dataset has price data for five businesses over four
quarters, and the `ms_weights` dataset contain weights that give the relative importance of these business in their respective industries. The goal of this example is to show how to build a typical price index with these data. Note that these data have a fairly realistic pattern of missing data, and are emblematic of the kinds of data used to measure inflation.

```r
head(ms_prices)
```

```
#>   period business product price
#> 1 202001       B1       1  1.14
#> 2 202001       B1       2    NA
#> 3 202001       B1       3  6.09
#> 4 202001       B2       4  6.23
#> 5 202001       B2       5  8.61
#> 6 202001       B2       6  6.40
```

```r
head(ms_weights)
```

```
#>   business classification weight
#> 1       B1             11    553
#> 2       B2             11    646
#> 3       B3             11    312
#> 4       B4             12    622
#> 5       B5             12    330
```

The first step to build a price index with these data is to make business-level indexes---so called elemental indexes---that serve as the building blocks for the industry-level indexes. The `elemental_index()` function makes elemental indexes using information on the change in price for the products sold by each business (price relatives) in each quarter. By default `elemental_index()` makes a Jevons index, but any bilateral generalized-mean index is possible. The only wrinkle is that price data here are in levels, and not changes, but the `price_relative()` function can make the necessary conversion.

```r
relatives <- with(ms_prices, price_relative(price, period, product))

elementals <- with(
  ms_prices,
  elemental_index(relatives, period, business, na.rm = TRUE)
)

elementals
```

```
#> Period-over-period price index for 4 levels over 4 time periods 
#>    202001    202002    202003   202004
#> B1      1 0.8949097 0.3342939      NaN
#> B2      1       NaN       NaN 2.770456
#> B3      1 2.0200036 1.6353355 0.537996
#> B4    NaN       NaN       NaN 4.576286
```

With the elemental indexes out of the way, it's time to make a price-index aggregation structure that maps each business to its position in the hierarchical industry structure. The only hiccup is unpacking the digit-wise classification for each businesses that defines the hierarchy. That's the job of the `expand_classification()` function.

```r
hierarchy <- with(
  ms_weights, 
  c(expand_classification(classification), list(business))
)

pias <- aggregation_structure(hierarchy, ms_weights$weight)

pias
```

```
#> Aggregation structure for 5 elemental aggregates with 2 levels
#> above the elemental aggregates 
#>   level1 level2 ea weight
#> 1      1     11 B1    553
#> 2      1     11 B2    646
#> 3      1     11 B3    312
#> 4      1     12 B4    622
#> 5      1     12 B5    330
```

It is now simple to aggregate the business-level indexes according to this aggregation structure with the `aggregate()` function and chain them together to get the evolution of prices over time.

```r
ms_index <- aggregate(ms_elemental, pias, na.rm = TRUE)

chain(ms_index)
```

```
#> Fixed-base price index for 8 levels over 4 time periods 
#>    202001    202002    202003    202004
#> 1       1 1.3007239 1.3827662 3.7815355
#> 11      1 1.3007239 1.3827662 2.1771866
#> 12      1 1.3007239 1.3827662 6.3279338
#> B1      1 0.8949097 0.2991629 0.4710366
#> B2      1 1.3007239 1.3827662 3.8308934
#> B3      1 2.0200036 3.3033836 1.7772072
#> B4      1 1.3007239 1.3827662 6.3279338
#> B5      1 1.3007239 1.3827662 6.3279338
```

# References