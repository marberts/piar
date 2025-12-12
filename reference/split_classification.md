# Split a hierarchical classification

Expand a character representation of a hierarchical classification to
make a price index aggregation structure by splitting along a delimiter.

## Usage

``` r
split_classification(x, split, ..., sep = ".", pad = NA)
```

## Arguments

- x:

  A character vector, or something that can be coerced into one, of
  codes/labels for a specific level in a classification (e.g., 5-digit
  COICOP).

- split:

  A regular expression to delineate and split the levels in `x`. See
  [`strsplit()`](https://rdrr.io/r/base/strsplit.html).

- ...:

  Additional argument to pass to
  [`strsplit()`](https://rdrr.io/r/base/strsplit.html).

- sep:

  A character used to delineate levels in `x` in the result. The default
  separates levels by '.'.

- pad:

  A string used to pad the shorter labels for an unbalanced
  classification. The default pads with NA.

## Value

A list with a entry for each level in `x` giving the "digits" that
represent each level in the hierarchy.

## See also

[`aggregation_structure()`](https://marberts.github.io/piar/reference/aggregation_structure.md)
to make a price-index aggregation structure.

[`expand_classification()`](https://marberts.github.io/piar/reference/expand_classification.md)
to expand a classification by the width of the levels.

## Examples

``` r
#' # A simple classification structure
#            1
#      |-----+-----|
#      11          12
#  |---+---|       |
#  111     112     121

split_classification(c("111", "112", "121"), "")
#> [[1]]
#> [1] "1" "1" "1"
#> 
#> [[2]]
#> [1] "1.1" "1.1" "1.2"
#> 
#> [[3]]
#> [1] "1.1.1" "1.1.2" "1.2.1"
#> 

# Useful if there are delimiters in the classification (like COICOP)

split_classification(c("01.1.1", "01.1.2", "01.2.1"), ".", fixed = TRUE)
#> [[1]]
#> [1] "01" "01" "01"
#> 
#> [[2]]
#> [1] "01.1" "01.1" "01.2"
#> 
#> [[3]]
#> [1] "01.1.1" "01.1.2" "01.2.1"
#> 
```
