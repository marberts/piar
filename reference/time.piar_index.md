# Get the time periods for a price index

Methods to get and set the time periods for a price index.

## Usage

``` r
# S3 method for class 'piar_index'
time(x, ...)

time(x) <- value

# S3 method for class 'piar_index'
time(x) <- value

set_time(x, value)

# S3 method for class 'piar_index'
start(x, ...)

# S3 method for class 'piar_index'
end(x, ...)

ntime(x)
```

## Arguments

- x:

  A price index, as made by, e.g.,
  [`elementary_index()`](https://marberts.github.io/piar/reference/elementary_index.md).

- ...:

  Not currently used.

- value:

  A character vector, or something that can be coerced into one, giving
  the replacement time periods for `x`.

## Value

[`time()`](https://rdrr.io/r/stats/time.html) returns a character vector
with the time periods for a price index.
[`start()`](https://rdrr.io/r/stats/start.html) and
[`end()`](https://rdrr.io/r/stats/start.html) return the first and last
time period.

`ntime()` returns the number of time periods, analogous to
[`nlevels()`](https://rdrr.io/r/base/nlevels.html).

The replacement method returns a copy of `x` with the time periods in
`value`. (`set_time()` is an alias that's easier to use with pipes.)

## See also

Other index methods:
[`[.piar_index()`](https://marberts.github.io/piar/reference/sub-.piar_index.md),
[`aggregate.piar_index`](https://marberts.github.io/piar/reference/aggregate.piar_index.md),
[`as.data.frame.piar_index()`](https://marberts.github.io/piar/reference/as.data.frame.piar_index.md),
[`as.ts.piar_index()`](https://marberts.github.io/piar/reference/as.ts.piar_index.md),
[`chain()`](https://marberts.github.io/piar/reference/chain.md),
[`contrib()`](https://marberts.github.io/piar/reference/contrib.md),
[`head.piar_index()`](https://marberts.github.io/piar/reference/head.piar_index.md),
[`is.na.piar_index()`](https://marberts.github.io/piar/reference/is.na.piar_index.md),
[`levels.piar_index()`](https://marberts.github.io/piar/reference/levels.piar_index.md),
[`mean.piar_index`](https://marberts.github.io/piar/reference/mean.piar_index.md),
[`merge.piar_index()`](https://marberts.github.io/piar/reference/merge.piar_index.md),
[`split.piar_index()`](https://marberts.github.io/piar/reference/split.piar_index.md),
[`stack.piar_index()`](https://marberts.github.io/piar/reference/stack.piar_index.md),
[`window.piar_index()`](https://marberts.github.io/piar/reference/window.piar_index.md)
