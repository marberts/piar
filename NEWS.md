# Version 0.7.1

## Improvements

- Added `interact_classifications()` to get the interaction of different
dimensions for a hierarchical classification.

# Version 0.7.0

## Significant changes

- Some arguments for `elemental_index()`, `as_index()`, `aggregate(index)`,
`mean(index)`, `vcov(index)`, `update(aggregation_structure)`, and `weights(aggregation_structure)` now need to be named (e.g., `na.rm`, `contrib`).
This helps to unify the signatures for several functions that had similar
arguments in different positions. In all cases these are arguments that are
not near the beginning of the function and should have probably been named
anyways.

- There are several bug fixes in this version that make non-backwards compatible
changes.

## Improvements

- Added examples for finding imputed index values to the vignette.

- `contrib()` gets a new argument `period` to control which time periods get
included in the contributions matrix (as documented).

- `contrib()` gets a new argument `pad` to control how the contributions matrix
is padded when products differ over time.

- Added `is.na()` and `anyNA()` methods to find missing values in an index
object.

- `index[i] <- value` now works when `i` is a matrix.

- `mean()` gets a new argument `contrib` to control if product contributions
are aggregated over subperiods.

- Added a `split()` method for index objects.

- `levels(aggregation_structure)` now returns a list of levels to denote
the position of each level in the hierarchy. Use `unlist()` to get the old
behavior.

## Bug fixes

- The default for `ea_only` has changed to `TRUE` when calling
`weights(aggregation_structure)` to fix a bug with the replacement method.

- Replacing an index value with `index[] <- value` when `value` is also an index
object now works correctly when `value` is recycled.

- Setting `stringsAsFactors = TRUE` in `as.data.frame(index)` now keeps the
correct ordering of the factor levels.

- `mean(index)` no longer returns an aggregate index when `r` differs from that
used to make `index`.

## Deprecations

- The `cols` argument for `as_index()` is deprecated and will be removed in a
future version.

# Version 0.6.0

## Significant changes

- The `[[` method for index objects has been removed as it created unexpected problems for little gain. `as.matrix(index)[[1, 1]]` is a more explicit and flexible way to get the same behavior as `index[[1, 1]]`.

- `aggregation_structure()` now orders the levels of an aggregation according to the order they appear in the data. Previously the levels were ordered lexicographically, except for the elemental aggregates. This can affect the order in which index values appear for an aggregate index.

There are a number of changes to the way product names are handled when making an index and extracting percent-change contributions.

- Names for price relatives now need to be unique within a time period for `elemental_index()`. The previous (undocumented) behavior was to only require that names be unique within a time period *and* elemental aggregate. This implies two non-backward compatible changes.

   - The default product names for `elemental_index()` now include the name of the elemental aggregate to conform to the above requirement.

   - Percent-change contributions from `contrib()` have simplified row names, as there is now no need to include index-level names to make product names unique.
   
- `contrib()` now always returns a matrix. Previously it would return `NULL` if there were no contributions for each level of the index.

- Rows for the contributions matrix are ordered according to product names so that they have a consistent ordering.

## Improvements

- Printing an index gives a textual description in addition to the matrix of index values. Printing an aggregation structure now gives a description and tabular representation instead of a list.

- There are now methods to set the levels and time periods of an index.

- Methods for index objects are now faster for larger indexes.

- `aggregate()` gains a new argument `contrib` that controls if percent-change contributions for elemental indexes are aggregated. The default maintains the current behavior of aggregating contributions if there are any.

- The class names for index objects have changed to fix a name conflict with `Matrix`. This means it's now possible to use `rsmatrix` with `piar`.

- The `as.matrix()` method for aggregation structures gains a new argument `sparse`. If `sparse = TRUE` then the aggregation matrix is a sparse, rather than dense, matrix. This option can also be used in the `vcov()` method for aggregate price indexes to improve performance for large indexes.

- Added the `carry_backwards()` function to do carry backwards (as opposed to carry forwards) imputation.

## Bug fixes

- Viewing index objects in the RStudio viewer longer gives an error.

- `is_direct_index()` is now exported.

- Replacing index values for an aggregate index no longer returns an aggregate index, as it may not be consistent in aggregation.

- Stacking two indexes now only returns an aggregate index if both indexes are themselves aggregate indexes. Previously it was possible to stack an aggregate index with a non-aggregate index to produce an aggregate index that was not consistent in aggregation.

# Version 0.5.0

## Significant changes

- `piar` now requires **R** >= 4.0.

- `is_chain_index()` has been removed; use `is_chainable_index()` instead.

- The first argument to `elemental_index()` is now `x`, not `rel`, to be consistent with the rest of the functions. Similarly, the first argument for `expand_classification()` is now `x`, not `class`.

## New features

- New functions `as_aggregation_structure()` and `is_aggregation_structure()` to coerce (usually) tabular data for aggregation weights into an aggregation structure, and test if an object is an aggregation structure.

- A method for `[[` for index objects to extract or replace a single index value.

- The weights for an aggregation structure can be replace with `weights(pias) <- vector`.

## Improvements

- The `levels()` replacement function now gives an error for indexes and aggregation structures, rather than adding a levels attribute that does nothing. 

- The `width` argument for `expand_classification()` now recycles a single value.

- Major overhaul of the documentation should make it easier to use.

- The object structure used to represent index object has been refined.

- Subscripting an index object is now much faster.

- It is now possible to update an aggregation structure with a non-aggregated index.

- Functions that accept a price index or an aggregation structure as an argument now attempt to coerce these arguments into an index object or aggregation structure object instead of throwing an error.

## Bug fixes

- Fixed a bug where creating elemental indexes with missing product names could produce a contributions matrix with the wrong number of products.

- It is now possible to chain an index with only one level.

- Subscripting an index with `NA`s or duplicate indices is no longer allowed.

# Version 0.4.0

## Significant changes

- The `vcov()` method for aggregate indexes now returns a matrix of variances instead of an array of covariances, as the covariances are usually misleading and unnecessary. In most cases elemental aggregates are sampled independently, in which case the covariances should be 0. This is not backwards compatible.

- The `vcov()` method for aggregate indexes is now much faster. The options for parallel computing have been removed, as they're unlikely to be useful for even large indexes.

- The `head()` and `tail()` methods for index objects now return an index object instead of a matrix, as documented.

## New features

- Added an `as.matrix()` method for pias objects. This makes it easy to aggregate a price index as a matrix operation; just matrix multiply the aggregation matrix with the elemental indexes.

- Added an `as.data.frame()` method for pias objects. This is useful for writing price-updated weights with, e.g., `write.csv()`.

- Added an `as_index()` method for data frames. This is faster and simpler than turning a data frame into an index with `elemental_index(df$value, df$period, df$level)`.

## Improvements

- The `chain` argument found in some of the methods for `elemental_index()` and `as_index()` to mark an index as chainable has been replaced by the more descriptive `chainable` argument. Partial matching of argument names means this is backwards compatible. 

- The `price_relative()` function no longer uses `gpindex::back_price()`, as this function is deprecated. This change will remove the harmless deprecation warning when using an older version of piar with a newer version of gpindex.

## Deprecated functions

- The `is_chain_index()` function is now deprecated, and replaced with the `is_chainable_index()` function.
