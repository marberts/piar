# Changelog

## piar (development version)

### Significant changes

- The internal representation of index objects has changed so that both
  the `index` and `contrib` components of an index are now matrices
  instead of lists. This simplifies the codebase and in some cases
  improves performance, but will break any coding that directly uses the
  internal components of an index.

## piar 0.8.3

CRAN release: 2025-09-17

### Improvements

- Both `aggregate(index)` and `mean(index)` get a new argument
  `duplicate_contrib` to control how product contributions are made when
  there are duplicate products.

- Coercing an index or an aggregation structure to a matrix now has
  dimension names. Same with the output from
  [`contrib()`](https://marberts.github.io/piar/reference/contrib.md).

- [`expand_classification()`](https://marberts.github.io/piar/reference/expand_classification.md)
  and
  [`split_classification()`](https://marberts.github.io/piar/reference/split_classification.md)
  get a new argument `pad` to better manage unbalanced classifications.

- It is no longer possible to make non-positive index values or use
  negative weights.

### Bug fixes

- Fixed description of how product contributions are combined across
  subperiods in
  [`?mean.piar_index`](https://marberts.github.io/piar/reference/mean.piar_index.md)
  ([\#51](https://github.com/marberts/piar/issues/51)).

- Converting a data frame with partial product contributions to an index
  object no longer gives an error
  ([\#52](https://github.com/marberts/piar/issues/52)).

- It is now an error to construct product contributions for a
  superlative index when missing/zero weights are mismatched
  ([\#59](https://github.com/marberts/piar/issues/59)).

## piar 0.8.2

CRAN release: 2025-03-19

- The vignette has been re-written, split into small examples that cover
  more topics, and should be easier to follow.

### Improvements

- Added
  [`set_levels()`](https://marberts.github.io/piar/reference/levels.piar_index.md),
  [`set_time()`](https://marberts.github.io/piar/reference/time.piar_index.md),
  and
  [`set_weights()`](https://marberts.github.io/piar/reference/weights.piar_aggregation_structure.md)
  to make it easier to replace levels, times, and weights with pipes.

- `contrib(index) <- value` can now be used to replace product
  contributions. The alias
  [`set_contrib()`](https://marberts.github.io/piar/reference/contrib.md)
  is easier to use with pipes.

- [`elementary_index()`](https://marberts.github.io/piar/reference/elementary_index.md)
  is now an alias for
  [`elemental_index()`](https://marberts.github.io/piar/reference/elementary_index.md)
  as this is more common in the literature.

- [`aggregate()`](https://rdrr.io/r/stats/aggregate.html) can now use
  two aggregation structures to make a superlative index.

- `cut(aggregation_structure)` can be used cut off the bottom/top of an
  aggregation structure. Works in conjunction with
  [`set_contrib_from_index()`](https://marberts.github.io/piar/reference/contrib.md)
  to calculate index-level contributions.

- Aggregation structures now preserve the names of their levels to work
  with [`cut()`](https://rdrr.io/r/base/cut.html). This means that
  `as.data.frame(aggregation_structure)` can produce different column
  names.

- `as.data.frame(index)` gets an option to make a list-column of
  percent-change contributions. `as_index(data.frame)` gets an analogous
  option to add contributions in a table to an index.

- `as.ts(index)` can now be used to turn an index into a regular time
  series and `as_index(ts)` can turn a time series into an index object.

- [`split_classification()`](https://marberts.github.io/piar/reference/split_classification.md)
  gives another way to generate an aggregation structure from a
  character vector.

### Bug fixes

- [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) methods
  now respect the signature of the generic. This allows row names to be
  set and prevents superfluous warnings when trying to use
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html); e.g., with
  [`write.csv()`](https://rdrr.io/r/utils/write.table.html)
  ([\#34](https://github.com/marberts/piar/issues/34)).

- Subscripting an index object with a length 0 vector is no longer an
  error ([\#48](https://github.com/marberts/piar/issues/48)).

## piar 0.8.1

CRAN release: 2024-09-12

### Significant changes

- [`elemental_index()`](https://marberts.github.io/piar/reference/elementary_index.md),
  [`price_relative()`](https://marberts.github.io/piar/reference/price_relative.md),
  [`shadow_price()`](https://marberts.github.io/piar/reference/impute_prices.md),
  [`carry_forward()`](https://marberts.github.io/piar/reference/impute_prices.md),
  and
  [`carry_backward()`](https://marberts.github.io/piar/reference/impute_prices.md)
  now have a formula interface to select the relevant, e.g., price
  variables from a data frame instead of using
  [`with()`](https://rdrr.io/r/base/with.html). This changes the
  signature of these functions, and may break old code if argument were
  not named.

  - `as_aggregation_structure(list)` and `mean(index)` now require the
    `weights` argument to be named to be consistent with other
    functions.

- The `aggregate_piar_index` class has been removed. This class was not
  well thought out, and added unnecessary restrictions and complications
  to certain functions. In most cases this has little impact on existing
  code, but does mean that the few functions related to aggregate
  indexes have been removed.

- The deprecated `cols` argument for `as_index(data.frame)` has been
  removed.

- The default window size for `mean(index)` now covers the entire index.
  This makes it so that [`mean()`](https://rdrr.io/r/base/mean.html)
  doesn’t assume an index is monthly, and makes it easier to use
  [`mean()`](https://rdrr.io/r/base/mean.html) with the new
  [`window()`](https://rdrr.io/r/stats/window.html) method for index
  object.

### Improvements

- [`as_index()`](https://marberts.github.io/piar/reference/as_index.md)
  gains a new argument `contrib` to add contributions to pre-computed
  indexes.

- Added
  [`contrib2DF()`](https://marberts.github.io/piar/reference/contrib.md)
  to extract percent-change contributions as a data frame.

- [`elemental_index()`](https://marberts.github.io/piar/reference/elementary_index.md),
  [`price_relative()`](https://marberts.github.io/piar/reference/price_relative.md),
  [`shadow_price()`](https://marberts.github.io/piar/reference/impute_prices.md),
  [`carry_forward()`](https://marberts.github.io/piar/reference/impute_prices.md),
  and
  [`carry_backward()`](https://marberts.github.io/piar/reference/impute_prices.md)
  now require the arguments for time periods, products, and elemental
  aggregates to be named so as to avoid accidentally changing the order
  of these arguments ([\#7](https://github.com/marberts/piar/issues/7),
  [@schneiderpy](https://github.com/schneiderpy)).

- Added
  [`interact_classifications()`](https://marberts.github.io/piar/reference/expand_classification.md)
  to get the interaction of different dimensions for a hierarchical
  classification.

- [`unchain()`](https://marberts.github.io/piar/reference/chain.md) gets
  an new argument `base` for better interaction with
  [`chain()`](https://marberts.github.io/piar/reference/chain.md) and
  [`rebase()`](https://marberts.github.io/piar/reference/chain.md).

- `aggregate(index)` gets a new argument `include_ea` to control whether
  elemental indexes are returned when aggregating.

- The [`summary()`](https://rdrr.io/r/base/summary.html) method for
  indexes has been changed to simply print a summary of the index,
  rather than try to summarized the index values.

- `aggregate(index)` now imputes percent-change contributions for
  missing index values when `na.rm = TRUE`. The previously undocumented
  behavior was to simply drop them.

- Added `window(index)` to extract a window of price indexes and
  `window(index) <- value` to replace them.

- The base period for
  [`rebase()`](https://marberts.github.io/piar/reference/chain.md) can
  now be a time period specifying a new base period. This makes it
  easier to rebase with pipes; e.g.,
  `index |> mean() |> rebase("202001")`.

- [`elemental_index()`](https://marberts.github.io/piar/reference/elementary_index.md)
  gets a new argument `product` to better control product names.

## piar 0.7.0

CRAN release: 2024-03-08

### Significant changes

- Some arguments for
  [`elemental_index()`](https://marberts.github.io/piar/reference/elementary_index.md),
  [`as_index()`](https://marberts.github.io/piar/reference/as_index.md),
  `aggregate(index)`, `mean(index)`, `vcov(index)`,
  `update(aggregation_structure)`, and `weights(aggregation_structure)`
  now need to be named (e.g., `na.rm`, `contrib`). This helps to unify
  the signatures for several functions that had similar arguments in
  different positions. In all cases these are arguments that are not
  near the beginning of the function and should have probably been named
  anyways.

- There are several bug fixes in this version that make non-backwards
  compatible changes.

### Improvements

- Added examples for finding imputed index values to the vignette.

- [`contrib()`](https://marberts.github.io/piar/reference/contrib.md)
  gets a new argument `period` to control which time periods get
  included in the contributions matrix (as documented).

- [`contrib()`](https://marberts.github.io/piar/reference/contrib.md)
  gets a new argument `pad` to control how the contributions matrix is
  padded when products differ over time.

- Added [`is.na()`](https://rdrr.io/r/base/NA.html) and
  [`anyNA()`](https://rdrr.io/r/base/NA.html) methods to find missing
  values in an index object.

- `index[i] <- value` now works when `i` is a matrix.

- [`mean()`](https://rdrr.io/r/base/mean.html) gets a new argument
  `contrib` to control if product contributions are aggregated over
  subperiods.

- Added a [`split()`](https://rdrr.io/r/base/split.html) method for
  index objects.

- `levels(aggregation_structure)` now returns a list of levels to denote
  the position of each level in the hierarchy. Use
  [`unlist()`](https://rdrr.io/r/base/unlist.html) to get the old
  behavior.

### Bug fixes

- The default for `ea_only` has changed to `TRUE` when calling
  `weights(aggregation_structure)` to fix a bug with the replacement
  method.

- Replacing an index value with `index[] <- value` when `value` is also
  an index object now works correctly when `value` is recycled.

- Setting `stringsAsFactors = TRUE` in `as.data.frame(index)` now keeps
  the correct ordering of the factor levels.

- `mean(index)` no longer returns an aggregate index when `r` differs
  from that used to make `index`.

### Deprecations

- The `cols` argument for
  [`as_index()`](https://marberts.github.io/piar/reference/as_index.md)
  is deprecated and will be removed in a future version.

## piar 0.6.0

CRAN release: 2023-11-19

### Significant changes

- The `[[` method for index objects has been removed as it created
  unexpected problems for little gain. `as.matrix(index)[[1, 1]]` is a
  more explicit and flexible way to get the same behavior as
  `index[[1, 1]]`.

- [`aggregation_structure()`](https://marberts.github.io/piar/reference/aggregation_structure.md)
  now orders the levels of an aggregation according to the order they
  appear in the data. Previously the levels were ordered
  lexicographically, except for the elemental aggregates. This can
  affect the order in which index values appear for an aggregate index.

There are a number of changes to the way product names are handled when
making an index and extracting percent-change contributions.

- Names for price relatives now need to be unique within a time period
  for
  [`elemental_index()`](https://marberts.github.io/piar/reference/elementary_index.md).
  The previous (undocumented) behavior was to only require that names be
  unique within a time period *and* elemental aggregate. This implies
  two non-backward compatible changes.

  - The default product names for
    [`elemental_index()`](https://marberts.github.io/piar/reference/elementary_index.md)
    now include the name of the elemental aggregate to conform to the
    above requirement.

  - Percent-change contributions from
    [`contrib()`](https://marberts.github.io/piar/reference/contrib.md)
    have simplified row names, as there is now no need to include
    index-level names to make product names unique.

- [`contrib()`](https://marberts.github.io/piar/reference/contrib.md)
  now always returns a matrix. Previously it would return `NULL` if
  there were no contributions for each level of the index.

- Rows for the contributions matrix are ordered according to product
  names so that they have a consistent ordering.

### Improvements

- Printing an index gives a textual description in addition to the
  matrix of index values. Printing an aggregation structure now gives a
  description and tabular representation instead of a list.

- There are now methods to set the levels and time periods of an index.

- Methods for index objects are now faster for larger indexes.

- [`aggregate()`](https://rdrr.io/r/stats/aggregate.html) gains a new
  argument `contrib` that controls if percent-change contributions for
  elemental indexes are aggregated. The default maintains the current
  behavior of aggregating contributions if there are any.

- The class names for index objects have changed to fix a name conflict
  with `Matrix`. This means it’s now possible to use `rsmatrix` with
  `piar`.

- The [`as.matrix()`](https://rdrr.io/r/base/matrix.html) method for
  aggregation structures gains a new argument `sparse`. If
  `sparse = TRUE` then the aggregation matrix is a sparse, rather than
  dense, matrix. This option can also be used in the
  [`vcov()`](https://rdrr.io/r/stats/vcov.html) method for aggregate
  price indexes to improve performance for large indexes.

- Added the `carry_backwards()` function to do carry backwards (as
  opposed to carry forwards) imputation.

### Bug fixes

- Viewing index objects in the RStudio viewer longer gives an error.

- [`is_direct_index()`](https://marberts.github.io/piar/reference/is_index.md)
  is now exported.

- Replacing index values for an aggregate index no longer returns an
  aggregate index, as it may not be consistent in aggregation.

- Stacking two indexes now only returns an aggregate index if both
  indexes are themselves aggregate indexes. Previously it was possible
  to stack an aggregate index with a non-aggregate index to produce an
  aggregate index that was not consistent in aggregation.

## piar 0.5.0

CRAN release: 2023-08-10

### Significant changes

- `piar` now requires **R** \>= 4.0.

- `is_chain_index()` has been removed; use
  [`is_chainable_index()`](https://marberts.github.io/piar/reference/is_index.md)
  instead.

- The first argument to
  [`elemental_index()`](https://marberts.github.io/piar/reference/elementary_index.md)
  is now `x`, not `rel`, to be consistent with the rest of the
  functions. Similarly, the first argument for
  [`expand_classification()`](https://marberts.github.io/piar/reference/expand_classification.md)
  is now `x`, not `class`.

### New features

- New functions
  [`as_aggregation_structure()`](https://marberts.github.io/piar/reference/as_aggregation_structure.md)
  and
  [`is_aggregation_structure()`](https://marberts.github.io/piar/reference/is_aggregation_structure.md)
  to coerce (usually) tabular data for aggregation weights into an
  aggregation structure, and test if an object is an aggregation
  structure.

- A method for `[[` for index objects to extract or replace a single
  index value.

- The weights for an aggregation structure can be replace with
  `weights(pias) <- vector`.

### Improvements

- The [`levels()`](https://rdrr.io/r/base/levels.html) replacement
  function now gives an error for indexes and aggregation structures,
  rather than adding a levels attribute that does nothing.

- The `width` argument for
  [`expand_classification()`](https://marberts.github.io/piar/reference/expand_classification.md)
  now recycles a single value.

- Major overhaul of the documentation should make it easier to use.

- The object structure used to represent index object has been refined.

- Subscripting an index object is now much faster.

- It is now possible to update an aggregation structure with a
  non-aggregated index.

- Functions that accept a price index or an aggregation structure as an
  argument now attempt to coerce these arguments into an index object or
  aggregation structure object instead of throwing an error.

### Bug fixes

- Fixed a bug where creating elemental indexes with missing product
  names could produce a contributions matrix with the wrong number of
  products.

- It is now possible to chain an index with only one level.

- Subscripting an index with `NA`s or duplicate indices is no longer
  allowed.

## piar 0.4.0

CRAN release: 2022-04-30

### Significant changes

- The [`vcov()`](https://rdrr.io/r/stats/vcov.html) method for aggregate
  indexes now returns a matrix of variances instead of an array of
  covariances, as the covariances are usually misleading and
  unnecessary. In most cases elemental aggregates are sampled
  independently, in which case the covariances should be 0. This is not
  backwards compatible.

- The [`vcov()`](https://rdrr.io/r/stats/vcov.html) method for aggregate
  indexes is now much faster. The options for parallel computing have
  been removed, as they’re unlikely to be useful for even large indexes.

- The [`head()`](https://rdrr.io/r/utils/head.html) and
  [`tail()`](https://rdrr.io/r/utils/head.html) methods for index
  objects now return an index object instead of a matrix, as documented.

### New features

- Added an [`as.matrix()`](https://rdrr.io/r/base/matrix.html) method
  for pias objects. This makes it easy to aggregate a price index as a
  matrix operation; just matrix multiply the aggregation matrix with the
  elemental indexes.

- Added an
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) method
  for pias objects. This is useful for writing price-updated weights
  with, e.g., [`write.csv()`](https://rdrr.io/r/utils/write.table.html).

- Added an
  [`as_index()`](https://marberts.github.io/piar/reference/as_index.md)
  method for data frames. This is faster and simpler than turning a data
  frame into an index with
  `elemental_index(df$value, df$period, df$level)`.

### Improvements

- The `chain` argument found in some of the methods for
  [`elemental_index()`](https://marberts.github.io/piar/reference/elementary_index.md)
  and
  [`as_index()`](https://marberts.github.io/piar/reference/as_index.md)
  to mark an index as chainable has been replaced by the more
  descriptive `chainable` argument. Partial matching of argument names
  means this is backwards compatible.

- The
  [`price_relative()`](https://marberts.github.io/piar/reference/price_relative.md)
  function no longer uses `gpindex::back_price()`, as this function is
  deprecated. This change will remove the harmless deprecation warning
  when using an older version of piar with a newer version of gpindex.

### Deprecated functions

- The `is_chain_index()` function is now deprecated, and replaced with
  the
  [`is_chainable_index()`](https://marberts.github.io/piar/reference/is_index.md)
  function.
