# Coerce an aggregation structure into a tabular form

Coerce a price index aggregation structure into an aggregation matrix,
or a data frame.

## Usage

``` r
# S3 method for class 'piar_aggregation_structure'
as.matrix(x, ..., sparse = FALSE)

# S3 method for class 'piar_aggregation_structure'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x:

  A price index aggregation structure, as made by
  [`aggregation_structure()`](https://marberts.github.io/piar/reference/aggregation_structure.md).

- ...:

  Not currently used for the matrix method. Extra arguments to
  [`as.data.frame.list()`](https://rdrr.io/r/base/as.data.frame.html)
  for the data frame method.

- sparse:

  Should the result be a sparse matrix from Matrix? This is faster for
  large aggregation structures. The default returns an ordinary dense
  matrix.

- row.names:

  See [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

- optional:

  Not currently used.

## Value

[`as.matrix()`](https://rdrr.io/r/base/matrix.html) represents an
aggregation structure as a matrix, such that multiplying with a (column)
vector of elementary indexes gives the aggregated index.

[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) takes an
aggregation structure and returns a data frame that could have generated
it.

## See also

[`as_aggregation_structure()`](https://marberts.github.io/piar/reference/as_aggregation_structure.md)
for coercing into an aggregation structure.

[`treemap::treemap()`](https://rdrr.io/pkg/treemap/man/treemap.html) and
[`data.tree::as.Node()`](https://rdrr.io/pkg/data.tree/man/as.Node.html)
for visualizing an aggregation structure.

Other aggregation structure methods:
[`cut.piar_aggregation_structure()`](https://marberts.github.io/piar/reference/cut.piar_aggregation_structure.md),
[`levels.piar_aggregation_structure()`](https://marberts.github.io/piar/reference/levels.piar_aggregation_structure.md),
[`update.piar_aggregation_structure()`](https://marberts.github.io/piar/reference/update.piar_aggregation_structure.md),
[`weights.piar_aggregation_structure()`](https://marberts.github.io/piar/reference/weights.piar_aggregation_structure.md)

## Examples

``` r
# A simple aggregation structure
#            1
#      |-----+-----|
#      11          12
#  |---+---|       |
#  111     112     121
#  (1)     (3)     (4)

aggregation_weights <- data.frame(
  level1 = c("1", "1", "1"),
  level2 = c("11", "11", "12"),
  ea     = c("111", "112", "121"),
  weight = c(1, 3, 4)
)

pias <- as_aggregation_structure(aggregation_weights)

as.matrix(pias)
#>       
#> levels   111   112 121
#>     1  0.125 0.375 0.5
#>     11 0.250 0.750 0.0
#>     12 0.000 0.000 1.0

all.equal(as.data.frame(pias), aggregation_weights)
#> [1] TRUE

if (FALSE) { # \dontrun{
# Visualize as a treemap.
treemap::treemap(
  aggregation_weights,
  index = names(aggregation_weights)[-4],
  vSize = "weight",
  title = "aggregation structure"
)

# Or turn into a more genereal tree object and plot.
aggregation_weights$pathString <- do.call(
  \(...) paste(..., sep = "/"),
  aggregation_weights[-4]
)
plot(data.tree::as.Node(aggregation_weights))
} # }
```
