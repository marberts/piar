#' Merge price indexes
#'
#' @description
#' Combine two price indexes with common time periods, merging together the
#' index values and percent-change contributions for each time period.
#'
#' This is useful for building up an index when different elemental aggregates
#' come from different sources of data, or use different index-number formulas.
#'
#' @name merge.piar_index
#' @aliases merge.piar_index
#' 
#' @param x A price index, as made by, e.g., [elemental_index()].
#' @param y A price index, or something that can coerced into one. If `x`
#' is a period-over-period index then `y` is coerced into a chainable
#' index; otherwise, `y` is coerced into a direct index.
#' @param ... Not currently used.
#'
#' @returns
#' A combined price index that inherits from the same class as `x`.
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
merge.chainable_piar_index <- function(x, y, ...) {
  y <- as_index(y, chainable = TRUE)
  NextMethod("merge")
}

#' @rdname merge.piar_index
#' @export
merge.direct_piar_index <- function(x, y, ...) {
  y <- as_index(y, chainable = FALSE)
  NextMethod("merge")
}

#' @export
merge.piar_index <- function(x, y, ...) {
  if (any(x$time != y$time)) {
    stop("'x' and 'y' must be indexes for the same time periods")
  }
  if (any(x$levels %in% y$levels)) {
    stop("the same levels cannot appear in both 'x' and 'y'")
  }
  x$index <- Map(c, x$index, y$index)
  x$contrib <- Map(c, x$contrib, y$contrib)
  x$levels <- c(x$levels, y$levels)
  x
}
