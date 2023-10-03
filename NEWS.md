# Version 0.6.0

## Significant changes

- The `[[` method for index objects has been removed as it created unexpected problems for little gain. A more explicit and flexible way to get the same behavior as `index[[1, 1]]` is `as.matrix(index)[[1, 1]]`.

- The class names for index objects have changed to fix a name conflict with `Matrix`. This means it's now possible to use `rsmatrix` with `piar`.

- Names for price relatives now need to be unique within a time period for `elemental_index()`. The previous (undocumented) behavior was to only require names be unique within a time period *and* elemental aggregate. This change has two non-backward compatible implications.

   - The default product names for `elemental_index()` now include the name of the elemental aggregate to conform to the above requirement.

   - Percent-change contributions from `contrib()` have simplified row names, as there is now no need to include index-level names to make product names unique.
   
- `contrib()` now always returns a matrix. Previously it would return `NULL` if there were no contributions for each level of the index.

## Improvements

- Printing an index gives a textual description in addition to the matrix of index values. Printing an aggregation structure now just gives a description instead of a list.

- There are now methods to set the levels and time periods of an index.

- Methods for index objects are now faster for larger indexes.

- `aggregate()` gains a new argument `contrib` that controls if percent-change contributions for elemental indexes are aggregated. The default maintains the current behavior of aggregating contributions if there are any.

## Bug fixes

- Viewing index objects in the RStudio viewer longer gives an error.

- `is_direct_index()` is now exported.

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
