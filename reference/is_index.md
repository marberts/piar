# Test if an object is a price index

Test if an object is a index object or a subclass of an index object.

## Usage

``` r
is_index(x)

is_chainable_index(x)

is_direct_index(x)
```

## Arguments

- x:

  An object to test.

## Value

`is_index()` returns `TRUE` if `x` inherits from
[`piar_index`](https://marberts.github.io/piar/reference/piar_index.md).

`is_chainable_index()` returns `TRUE` if `x` inherits from
[`chainable_piar_index`](https://marberts.github.io/piar/reference/piar_index.md).

`is_direct_index()` returns `TRUE` if `x` inherits from
[`direct_piar_index`](https://marberts.github.io/piar/reference/piar_index.md).
