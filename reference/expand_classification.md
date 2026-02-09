# Expand a hierarchical classification

Expand a character representation of a hierarchical classification to
make a price index aggregation structure. Expanded classifications be
interacted together to get all combinations of aggregation structures.

## Usage

``` r
expand_classification(x, width = 1L, pad = NA)

interact_classifications(..., sep = ":")
```

## Arguments

- x:

  A character vector, or something that can be coerced into one, of
  codes/labels for a specific level in a classification (e.g., 5-digit
  COICOP, 5-digit NAICS, 4-digit SIC).

- width:

  An integer vector that gives the width of each digit in `x`. A single
  value is recycled to span the longest element in `x`. This cannot
  contain NAs. The default assumes each digit has a width of 1, as in
  the NAICS, NAPCS, and SIC classifications.

- pad:

  A string used to pad the shorter labels for an unbalanced
  classification. The default pads with NA.

- ...:

  Lists of character vectors that give the codes/labels for each level
  of the classification, ordered so that moving down the list goes down
  the hierarchy (as made by `expand_classification()`).

- sep:

  A character used to combine codes/labels across elements of `...`. The
  default uses `":"`.

## Value

`expand_classification()` returns a list with a entry for each level in
`x` giving the "digits" that represent each level in the hierarchy.

`interact_classfications()` returns a list of lists with the same
structure as `expand_classification()`.

## See also

[`aggregation_structure()`](https://marberts.github.io/piar/reference/aggregation_structure.md)
to make a price-index aggregation structure.

[`split_classification()`](https://marberts.github.io/piar/reference/split_classification.md)
to expand a classification by splitting along a delimiter.

[`combine_classifications()`](https://marberts.github.io/piar/reference/combine_classifications.md)
for combining multiple hierarchical classifications.

`csh_from_digits()` in the accumulate package for different handling of
unbalanced classifications.

## Examples

``` r
# A simple classification structure
#            1
#      |-----+-----|
#      11          12
#  |---+---|       |
#  111     112     121

expand_classification(c("111", "112", "121"))
#> [[1]]
#> [1] "1" "1" "1"
#> 
#> [[2]]
#> [1] "11" "11" "12"
#> 
#> [[3]]
#> [1] "111" "112" "121"
#> 

# Expanding more complex classifications
# ... if last 'digit' is either TA or TS

expand_classification(
  c("111TA", "112TA", "121TS"),
  width = c(1, 1, 1, 2)
)
#> [[1]]
#> [1] "1" "1" "1"
#> 
#> [[2]]
#> [1] "11" "11" "12"
#> 
#> [[3]]
#> [1] "111" "112" "121"
#> 
#> [[4]]
#> [1] "111TA" "112TA" "121TS"
#> 

# ... if first 'digit' is either 11 or 12

expand_classification(c("111", "112", "121"), width = c(2, 1))
#> [[1]]
#> [1] "11" "11" "12"
#> 
#> [[2]]
#> [1] "111" "112" "121"
#> 

# ...if there are delimiters in the classification (like COICOP)

expand_classification(c("01.1.1", "01.1.2", "01.2.1"), width = 2)
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
