# Version 0.5.0

## Significant changes

- Requires **R** >= 4.0.

- `is_chain_index()` has been removed; use `is_chainable_index()` instead.

- The first argument to `elemental_index()` is now `x`, not `rel`, to be consistent with the rest of the functions.

## New features

- New functions `as_aggregation_structure()` and `is_aggregate_index()` to coerce (usually) tabular data for aggregation weights into an aggregation structure, and test if an object is an aggregation structure.

- A method for `[[` for index objects to extract a single index value.

- The weights for an aggregation structure can be replace with `weights(pias) <- vector`.

## Improvements

- The `levels()` replacement function now gives an error for indexes and aggregation structures, rather than adding a levels attribute that does nothing. 

- The `width` argument for `expand_classification()` now recycles with a single value.

- Overhaul of the documentation.

- The object structure used to represent index object has been refined.

## Bug fixes

- Fixed a bug where creating elemental indexes with missing product names could produce a contributions matrix with the wrong number of products.

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
