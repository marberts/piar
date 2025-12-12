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

  A list with an entry for each period in `time` that gives a vector of
  index values for each level in `levels`.

- contrib:

  A list with an entry for each period in `time`, which itself contains
  a list with an entry for each level in `levels` with a named vector
  that gives the percent-change contribution for each price relative.

- levels:

  A character vector giving the levels of the index.

- time:

  A character vector giving the time periods for the index.

The `chainable_piar_index` and `direct_piar_index` subclasses have the
same structure as the `piar_index` class, but differ in the methods used
to manipulate the indexes.
