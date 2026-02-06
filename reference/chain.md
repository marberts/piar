# Chain and rebase a price index

Chain a period-over-period index by taking the cumulative product of its
values to turn it into a fixed-base (direct) index.

Unchain a fixed-base index by dividing its values for successive periods
to get a period-over-period index.

Rebase a fixed-base index by dividing its values with the value of the
index in the new base period.

## Usage

``` r
chain(x, ...)

# Default S3 method
chain(x, ...)

# S3 method for class 'chainable_piar_index'
chain(x, ..., link = NULL)

unchain(x, ...)

# Default S3 method
unchain(x, ...)

# S3 method for class 'direct_piar_index'
unchain(x, ..., base = NULL)

rebase(x, ...)

# Default S3 method
rebase(x, ...)

# S3 method for class 'direct_piar_index'
rebase(x, ..., base = NULL)
```

## Arguments

- x:

  A price index, as made by, e.g.,
  [`elementary_index()`](https://marberts.github.io/piar/reference/elementary_index.md).

- ...:

  Further arguments passed to or used by methods.

- link:

  A numeric vector, or something that can coerced into one, of link
  values for each level in `x`. The default is equivalent to a vector of
  1s so that no linking is done.

- base:

  A numeric vector, or something that can coerced into one, of
  base-period index values for each level in `x`. The default is a
  equivalent to a vector of 1s so that the base period remains the same.
  If `base` is a length-one character vector giving a time period of `x`
  then the index values for this time period are used as the base-period
  values.

## Value

`chain()` and `rebase()` return a fixed-base index that inherits from
[`direct_piar_index`](https://marberts.github.io/piar/reference/piar_index.md).

`unchain()` returns a period-over-period index that inherits from
[`chainable_piar_index`](https://marberts.github.io/piar/reference/piar_index.md).

## Details

The default methods attempt to coerce `x` into an index with
[`as_index()`](https://marberts.github.io/piar/reference/as_index.md)
prior to chaining/unchaining/rebasing.

Chaining an index takes the cumulative product of the index values for
each level; this is roughly the same as
`t(apply(as.matrix(x), 1, cumprod)) * link`. Unchaining does the
opposite, so these are inverse operations. Note that unchaining a
period-over-period index does nothing, as does chaining a fixed-base
index.

Rebasing a fixed-base index divides the values for each level of this
index by the corresponding values for each level in the new base period.
It's roughly the same as `as.matrix(x) / base`. Like unchaining,
rebasing a period-over-period index does nothing.

Percent-change contributions are removed when
chaining/unchaining/rebasing an index as it's not usually possible to
update them correctly.

## See also

Other index methods:
[`[.piar_index()`](https://marberts.github.io/piar/reference/sub-.piar_index.md),
[`aggregate.piar_index`](https://marberts.github.io/piar/reference/aggregate.piar_index.md),
[`as.data.frame.piar_index()`](https://marberts.github.io/piar/reference/as.data.frame.piar_index.md),
[`as.ts.piar_index()`](https://marberts.github.io/piar/reference/as.ts.piar_index.md),
[`contrib()`](https://marberts.github.io/piar/reference/contrib.md),
[`head.piar_index()`](https://marberts.github.io/piar/reference/head.piar_index.md),
[`is.na.piar_index()`](https://marberts.github.io/piar/reference/is.na.piar_index.md),
[`levels.piar_index()`](https://marberts.github.io/piar/reference/levels.piar_index.md),
[`mean.piar_index`](https://marberts.github.io/piar/reference/mean.piar_index.md),
[`merge.piar_index()`](https://marberts.github.io/piar/reference/merge.piar_index.md),
[`split.piar_index()`](https://marberts.github.io/piar/reference/split.piar_index.md),
[`stack.piar_index()`](https://marberts.github.io/piar/reference/stack.piar_index.md),
[`time.piar_index()`](https://marberts.github.io/piar/reference/time.piar_index.md),
[`window.piar_index()`](https://marberts.github.io/piar/reference/window.piar_index.md)

## Examples

``` r
index <- as_index(matrix(1:9, 3))

# Make period 0 the fixed base period

chain(index)
#> Fixed-base price index for 3 levels over 3 time periods 
#>       time
#> levels 1  2   3
#>      1 1  4  28
#>      2 2 10  80
#>      3 3 18 162

# Chaining and unchaining reverse each other

all.equal(index, unchain(chain(index)))
#> [1] TRUE

# Change the base period to period 2 (note the
# loss of information for period 0)

index <- chain(index)
rebase(index, base = index[, 2])
#> Fixed-base price index for 3 levels over 3 time periods 
#>       time
#> levels         1 2 3
#>      1 0.2500000 1 7
#>      2 0.2000000 1 8
#>      3 0.1666667 1 9
```
