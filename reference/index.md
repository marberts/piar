# Package index

## Calculate price indexes

Functions to make elementary and aggregated price indexes.

- [`elementary_index()`](https://marberts.github.io/piar/reference/elementary_index.md)
  [`elemental_index()`](https://marberts.github.io/piar/reference/elementary_index.md)
  : Make elementary/elemental price indexes
- [`as_index()`](https://marberts.github.io/piar/reference/as_index.md)
  : Coerce to a price index
- [`aggregate(`*`<chainable_piar_index>`*`)`](https://marberts.github.io/piar/reference/aggregate.piar_index.md)
  [`aggregate(`*`<direct_piar_index>`*`)`](https://marberts.github.io/piar/reference/aggregate.piar_index.md)
  : Aggregate elementary price indexes
- [`mean(`*`<chainable_piar_index>`*`)`](https://marberts.github.io/piar/reference/mean.piar_index.md)
  [`mean(`*`<direct_piar_index>`*`)`](https://marberts.github.io/piar/reference/mean.piar_index.md)
  : Aggregate a price index over subperiods

## Manipulate price indexes

Helpful functions for working with price indexes.

- [`chain()`](https://marberts.github.io/piar/reference/chain.md)
  [`unchain()`](https://marberts.github.io/piar/reference/chain.md)
  [`rebase()`](https://marberts.github.io/piar/reference/chain.md) :
  Chain and rebase a price index
- [`merge(`*`<chainable_piar_index>`*`)`](https://marberts.github.io/piar/reference/merge.piar_index.md)
  [`merge(`*`<direct_piar_index>`*`)`](https://marberts.github.io/piar/reference/merge.piar_index.md)
  : Merge price indexes
- [`stack(`*`<chainable_piar_index>`*`)`](https://marberts.github.io/piar/reference/stack.piar_index.md)
  [`stack(`*`<direct_piar_index>`*`)`](https://marberts.github.io/piar/reference/stack.piar_index.md)
  [`unstack(`*`<chainable_piar_index>`*`)`](https://marberts.github.io/piar/reference/stack.piar_index.md)
  [`unstack(`*`<direct_piar_index>`*`)`](https://marberts.github.io/piar/reference/stack.piar_index.md)
  : Stack price indexes
- [`` `[`( ``*`<piar_index>`*`)`](https://marberts.github.io/piar/reference/sub-.piar_index.md)
  [`` `[<-`( ``*`<piar_index>`*`)`](https://marberts.github.io/piar/reference/sub-.piar_index.md)
  : Extract index values
- [`window(`*`<piar_index>`*`)`](https://marberts.github.io/piar/reference/window.piar_index.md)
  [`` `window<-`( ``*`<piar_index>`*`)`](https://marberts.github.io/piar/reference/window.piar_index.md)
  : Window a price index
- [`head(`*`<piar_index>`*`)`](https://marberts.github.io/piar/reference/head.piar_index.md)
  [`tail(`*`<piar_index>`*`)`](https://marberts.github.io/piar/reference/head.piar_index.md)
  : Return the first/last parts of an index
- [`contrib()`](https://marberts.github.io/piar/reference/contrib.md)
  [`contrib2DF()`](https://marberts.github.io/piar/reference/contrib.md)
  [`` `contrib<-`() ``](https://marberts.github.io/piar/reference/contrib.md)
  [`set_contrib()`](https://marberts.github.io/piar/reference/contrib.md)
  [`set_contrib_from_index()`](https://marberts.github.io/piar/reference/contrib.md)
  : Extract percent-change contributions
- [`split(`*`<piar_index>`*`)`](https://marberts.github.io/piar/reference/split.piar_index.md)
  [`` `split<-`( ``*`<piar_index>`*`)`](https://marberts.github.io/piar/reference/split.piar_index.md)
  : Split an index into groups
- [`levels(`*`<piar_index>`*`)`](https://marberts.github.io/piar/reference/levels.piar_index.md)
  [`` `levels<-`( ``*`<piar_index>`*`)`](https://marberts.github.io/piar/reference/levels.piar_index.md)
  [`set_levels()`](https://marberts.github.io/piar/reference/levels.piar_index.md)
  : Get the levels for a price index
- [`time(`*`<piar_index>`*`)`](https://marberts.github.io/piar/reference/time.piar_index.md)
  [`` `time<-`() ``](https://marberts.github.io/piar/reference/time.piar_index.md)
  [`set_time()`](https://marberts.github.io/piar/reference/time.piar_index.md)
  [`start(`*`<piar_index>`*`)`](https://marberts.github.io/piar/reference/time.piar_index.md)
  [`end(`*`<piar_index>`*`)`](https://marberts.github.io/piar/reference/time.piar_index.md)
  [`ntime()`](https://marberts.github.io/piar/reference/time.piar_index.md)
  : Get the time periods for a price index
- [`as.data.frame(`*`<piar_index>`*`)`](https://marberts.github.io/piar/reference/as.data.frame.piar_index.md)
  [`as.matrix(`*`<piar_index>`*`)`](https://marberts.github.io/piar/reference/as.data.frame.piar_index.md)
  : Coerce an index into a tabular form
- [`as.ts(`*`<piar_index>`*`)`](https://marberts.github.io/piar/reference/as.ts.piar_index.md)
  : Coerce an index into a time series
- [`is.na(`*`<piar_index>`*`)`](https://marberts.github.io/piar/reference/is.na.piar_index.md)
  [`anyNA(`*`<piar_index>`*`)`](https://marberts.github.io/piar/reference/is.na.piar_index.md)
  : Missing values in a price index

## Manipulate prices

Helpful functions for working with prices.

- [`shadow_price()`](https://marberts.github.io/piar/reference/impute_prices.md)
  [`carry_forward()`](https://marberts.github.io/piar/reference/impute_prices.md)
  [`carry_backward()`](https://marberts.github.io/piar/reference/impute_prices.md)
  : Impute missing prices
- [`price_relative()`](https://marberts.github.io/piar/reference/price_relative.md)
  : Calculate period-over-period price relatives

## Aggregation structure

Functions to make and work with price-index aggregation structures.

- [`aggregation_structure()`](https://marberts.github.io/piar/reference/aggregation_structure.md)
  : Make a price index aggregation structure
- [`expand_classification()`](https://marberts.github.io/piar/reference/expand_classification.md)
  [`interact_classifications()`](https://marberts.github.io/piar/reference/expand_classification.md)
  : Expand a hierarchical classification
- [`split_classification()`](https://marberts.github.io/piar/reference/split_classification.md)
  : Split a hierarchical classification
- [`combine_classifications()`](https://marberts.github.io/piar/reference/combine_classifications.md)
  : Combine hierarchical classifications
- [`as_aggregation_structure()`](https://marberts.github.io/piar/reference/as_aggregation_structure.md)
  : Coerce to an aggregation structure
- [`as.matrix(`*`<piar_aggregation_structure>`*`)`](https://marberts.github.io/piar/reference/as.matrix.piar_aggregation_structure.md)
  [`as.data.frame(`*`<piar_aggregation_structure>`*`)`](https://marberts.github.io/piar/reference/as.matrix.piar_aggregation_structure.md)
  : Coerce an aggregation structure into a tabular form
- [`weights(`*`<piar_aggregation_structure>`*`)`](https://marberts.github.io/piar/reference/weights.piar_aggregation_structure.md)
  [`` `weights<-`() ``](https://marberts.github.io/piar/reference/weights.piar_aggregation_structure.md)
  [`set_weights()`](https://marberts.github.io/piar/reference/weights.piar_aggregation_structure.md)
  : Get the weights for an aggregation structure
- [`levels(`*`<piar_aggregation_structure>`*`)`](https://marberts.github.io/piar/reference/levels.piar_aggregation_structure.md)
  : Get the levels for an aggregation structure
- [`update(`*`<piar_aggregation_structure>`*`)`](https://marberts.github.io/piar/reference/update.piar_aggregation_structure.md)
  : Update an aggregation structure
- [`cut(`*`<piar_aggregation_structure>`*`)`](https://marberts.github.io/piar/reference/cut.piar_aggregation_structure.md)
  : Cut an aggregation structure
- [`is_aggregation_structure()`](https://marberts.github.io/piar/reference/is_aggregation_structure.md)
  : Test if an object is an aggregation structure

## Index objects

The model used to represent price indexes.

- [`piar_index`](https://marberts.github.io/piar/reference/piar_index.md)
  [`chainable_piar_index`](https://marberts.github.io/piar/reference/piar_index.md)
  [`direct_piar_index`](https://marberts.github.io/piar/reference/piar_index.md)
  : Price index objects
- [`is_index()`](https://marberts.github.io/piar/reference/is_index.md)
  [`is_chainable_index()`](https://marberts.github.io/piar/reference/is_index.md)
  [`is_direct_index()`](https://marberts.github.io/piar/reference/is_index.md)
  : Test if an object is a price index

## Sample data

- [`price_data`](https://marberts.github.io/piar/reference/price_data.md)
  [`ms_prices`](https://marberts.github.io/piar/reference/price_data.md)
  [`ms_weights`](https://marberts.github.io/piar/reference/price_data.md)
  [`fs_prices`](https://marberts.github.io/piar/reference/price_data.md)
  [`fs_weights`](https://marberts.github.io/piar/reference/price_data.md)
  : Price data
