#' Merge price indexes
#'
#' @description
#' Combine two price indexes with common time periods, merging together the
#' index values and percent-change contributions for each time period.
#'
#' This is useful for building up an index when different elemental aggregates
#' come from different sources of data, or use different index-number formulas.
#'
#' @param x A price index, as made by, e.g., [elemental_index()].
#' @param y A price index, or something that can coerced into one. If `x`
#' is a period-over-period index then `y` is coerced into a chainable
#' index; otherwise, `y` is coerced into a direct index.
#' @param ... Not currently used.
#'
#' @returns
#' A price index that inherits from [`chainable_piar_index`] if `x` is a
#' period-over-period index, or [`direct_piar_index`] if `x` is a fixed-base
#' index. It is not generally possible to merge aggregated indexes, as this
#' would change the aggregation structure, so merging does not return an
#' aggregated index.
#'
#' @examples
#' index1 <- as_index(matrix(1:6, 2))
#' 
#' index2 <- index1
#' levels(index2) <- 3:4
#'
#' merge(index1, index2)
#'
#' @family index methods
#' @export
merge.piar_index <- function(x, y, ...) {
  if (!identical(x$time, y$time)) {
    stop("'x' and 'y' must be indexes for the same time periods")
  }
  if (any(x$levels %in% y$levels)) {
    stop("the same levels cannot appear in both 'x' and 'y'")
  }
  x$index <- Map(c, x$index, y$index)
  x$contrib <- Map(c, x$contrib, y$contrib)
  # it's safe to use c() and not union() because there can't be duplicate levels
  x$levels <- c(x$levels, y$levels)
  validate_piar_index(x)
}

#' @export
merge.aggregate_piar_index <- function(x, y, ...) {
  x <- new_piar_index(
    x$index, x$contrib, x$levels, x$time,
    is_chainable_index(x)
  )
  NextMethod("merge")
}

#' @export
merge.chainable_piar_index <- function(x, y, ...) {
  y <- as_index(y, chainable = TRUE)
  NextMethod("merge")
}

#' @export
merge.direct_piar_index <- function(x, y, ...) {
  y <- as_index(y, chainable = FALSE)
  NextMethod("merge")
}
