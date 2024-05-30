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
 - name: Statistics Canada, Ottawa, ON, Canada
   index: 1
date: 9 April 2024
bibliography: paper.bib
---

# Summary

The systematic inflation of prices for goods and services over time is a
ubiquitous feature of modern economies. A price index is the main empirical tool
to measure changes in prices over time, and consequently price indexes are a
core macroeconomic statistic produced by national statistical agencies to
measure and study inflation. Price indexes are often made with a two-step procedure---especially those coming from national statistical agencies---where period-over-period indexes are calculated for a large collection of well-defined
goods and services at each point in time and aggregated according to a
hierarchical structure [@ppimanual; @cpimanual]. These period-over-period
indexes can then be chained together to form a collection of time series that
give the evolution of prices in the economy with respect to a fixed point in
time.

`piar` is an R [@R] package for aggregating price indexes. It contains a
collection of functions that revolve around the usual two-step work flow for
computing price indexes, making it easy to build large, hierarchical indexes
using the methods described in the literature
[e.g., @balk2008; @cpimanual; @vonderlippe2001]. `piar` is designed to be useful
for both researching new sources of data and methods to construct price indexes,
and the regular production of price statistics. It is currently used to produce
several price indexes at Statistics Canada [e.g., @CMSPI; @FHMCFSPI].

# Statement of need

`piar` fills a gap in the open-source ecosystem for measuring inflation by
providing a tool to build the kinds of large, hierarchical price indexes made by
national statistical agencies. There are several R and Python packages for
accessing price indexes published by statistical agencies
[e.g., @vonbergmann2021; @welsh2024], but these are not suitable for computing
new indexes or researching new methods and data sources to measure inflation.   

To compute a price index, the `micEconIndex` [@henningsen2022] and
`IndexNumbers` [@saavedra2021] packages implement text-book index-number
formulas to measure the change in prices over time for a collection of goods and
services. These methods, however, are seldom directly used in practice; for
example, most text-book index-number formulas use information on both prices and 
quantities over time, but quantity information is rarely available in most
cases. The `IndexNumR` [@white2023] and `PriceIndices` [@bialek2024] packages
implement a variety of more sophisticated methods to build price indexes with high-frequency retail scanner data (in addition to the text-book methods).
Similarly, the `hpiR` [@krausse2020] and `rsmatrix` [@martin2023] packages
implement methods that are applicable to housing data. Although these more
advanced methods are directly used in practice to make price indexes for certain
types of goods and services, they are not suitable for constructing the
conventional price indexes that cover a wide range of products or industries.

In contrast to existing tools, `piar` provides a flexible API to build large,
hierarchical price indexes over time in a way that is suitable for both the data
and methods used to make large-scale measures of inflation. Part of this
flexible design means that pre-computed indexes can serve as an input to a
larger index, alongside other sources of price data (e.g., survey data), and so
`piar` complements existing tools in R by providing a framework for integrating
different index-number methods with heterogeneous sources of data.

# Example

The goal of this example is to illustrate some of the core features of `piar` by
building a typical industry price index with synthetic data. The built-in
`ms_prices` dataset has random price data for five businesses over four
quarters, and the `ms_weights` dataset contain weights that give the relative
importance of these businesses in their respective industries. Note that these
data have a fairly realistic pattern of missing data, and, although small, are
emblematic of the kinds of survey data used to measure inflation.

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

The first step to build a price index with these data is to make business-level indexes---so called elemental indexes---that serve as the building blocks for
the industry-level indexes. The `elemental_index()` function makes elemental
indexes using information on the change in price for the products sold by each
business (price relatives) in each quarter. By default `elemental_index()` makes
a Jevons index, but any bilateral generalized-mean index is possible. Note that
price data here are in levels, and not changes, but the
`price_relative()` function can make the necessary conversion.

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

With the elemental indexes out of the way, it's time to make a price-index
aggregation structure that maps each business to its position in the
hierarchical industry structure. Each business has a two-digit industry
classification that's first unpacked with the `expand_classification()` function
to make the aggregation hierarchy and then combined with the weights to make
an aggregation structure.

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

It is now simple to aggregate the business-level indexes according to this
aggregation structure with the `aggregate()` function and chain them together to
get the evolution of prices over time.

```r
ms_index <- aggregate(elementals, pias, na.rm = TRUE)

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

There are a variety of methods to manipulate the index objects that come from
these computations in order to compose more complex workflows for building price
indexes.

# Acknowledgements

I am grateful for many useful conversations with the research staff at
Statistics Canada, and in particular Xin Ha, that have improved `piar`.

The views and opinions expressed in this paper are my own and do
not necessarily reflect those of the Government of Canada.

# References