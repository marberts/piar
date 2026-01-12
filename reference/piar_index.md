# Price index objects

There are several classes to represent price indexes.

- All indexes inherit from the `piar_index` virtual class.

- Period-over-period indexes that can be chained over time inherit from
  `chainable_piar_index`.

- Fixed-base indexes inherit from `direct_piar_index`.

## Details

The `piar_index` object is a list-S3 class with the following
components:

- index:

  A matrix of index values with a column for each period in `time` and a
  row for each level in `levels`.

- contrib:

  A list-matrix containing named vectors that give the percent-change
  contributions for each price relative with a column for each time
  period in `time` and a row for each level in `levels`.

- levels:

  A character vector giving the levels of the index.

- time:

  A character vector giving the time periods for the index.

The `chainable_piar_index` and `direct_piar_index` subclasses have the
same structure as the `piar_index` class, but differ in the methods used
to manipulate the indexes.
