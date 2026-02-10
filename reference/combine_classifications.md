# Combine hierarchical classifications

Combine hierarchical classifications by stacking one classification
after another.

## Usage

``` r
combine_classifications(..., sep = ".")
```

## Arguments

- ...:

  A collection of lists, one for each classification, each giving the
  "digits" that represent each level in the hierarchy, as made by
  [`expand_classification()`](https://marberts.github.io/piar/reference/expand_classification.md)
  or
  [`split_classification()`](https://marberts.github.io/piar/reference/split_classification.md).

- sep:

  A character used to separate the classifications in `...`. The default
  separates levels across classifications by `"."`.

## Value

A list with a entry for each level in the combined classification.

## Examples

``` r
# Combine an unbalanced industry classification with a balanced
# geographic classification

industry <- c("111", "112", "12")
region <- c("11", "21", "22")

combine_classifications(
  expand_classification(industry, pad = "0"),
  expand_classification(region)
)
#> [[1]]
#> [1] "1" "1" "1"
#> 
#> [[2]]
#> [1] "11" "11" "12"
#> 
#> [[3]]
#> [1] "111" "112" "120"
#> 
#> [[4]]
#> [1] "111.1" "112.2" "120.2"
#> 
#> [[5]]
#> [1] "111.11" "112.21" "120.22"
#> 
```
