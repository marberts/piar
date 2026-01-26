#' Price index objects
#'
#' There are several classes to represent price indexes.
#' - All indexes inherit from the `piar_index` virtual class.
#' - Period-over-period indexes that can be chained over time inherit from
#' `chainable_piar_index`.
#' - Fixed-base indexes inherit from `direct_piar_index`.
#'
#' The `piar_index` object is a list-S3 class with the following
#' components:
#' \describe{
#' \item{index}{A matrix of index values with a column for each period in `time`
#' and a row for each level in `levels`.}
#' \item{contrib}{A list-matrix containing named vectors that give the
#' percent-change contributions for each price relative with a column for each
#' time period in `time` and a row for each level in `levels`.}
#' \item{levels}{A character vector giving the levels of the index.}
#' \item{time}{A character vector giving the time periods for the index.}
#' }
#'
#' The `chainable_piar_index` and `direct_piar_index` subclasses have
#' the same structure as the `piar_index` class, but differ in the methods
#' used to manipulate the indexes.
#'
#' @name piar_index
#' @aliases piar_index chainable_piar_index direct_piar_index
#'
NULL

#---- Class generator ----
new_piar_index <- function(index, contrib, levels, time, chainable) {
  stopifnot(is.double(index) && is.matrix(index))
  stopifnot(is.list(contrib) && is.matrix(contrib))
  stopifnot(is.character(levels))
  stopifnot(is.character(time))
  res <- list(index = index, contrib = contrib, levels = levels, time = time)
  type <- if (chainable) "chainable_piar_index" else "direct_piar_index"
  structure(res, class = c(type, "piar_index"))
}

piar_index <- function(index, contrib, levels, time, chainable) {
  index <- unname(index)
  contrib <- unname(contrib)
  levels <- as.character(levels)
  time <- as.character(time)
  validate_piar_index(
    new_piar_index(index, contrib, levels, time, chainable)
  )
}

#---- Validator ----
validate_levels <- function(x) {
  if (length(x$levels) == 0L) {
    stop("cannot make an index with no levels")
  }
  if (missing_names(x$levels)) {
    stop("cannot make an index with missing or length-zero levels")
  }
  if (anyDuplicated(x$levels)) {
    stop("cannot make an index with duplicate levels")
  }
  invisible(x)
}

validate_time <- function(x) {
  if (length(x$time) == 0L) {
    stop("cannot make an index with no time periods")
  }
  if (missing_names(x$time)) {
    stop("cannot make an index with missing or length-zero time periods")
  }
  if (anyDuplicated(x$time)) {
    stop("cannot make an index with duplicate time periods")
  }
  invisible(x)
}

validate_index_values <- function(x) {
  if (ncol(x$index) != length(x$time)) {
    stop("number of time periods does not agree with number of index values")
  }
  if (nrow(x$index) != length(x$levels)) {
    stop("number of levels does not agree with number of index values")
  }
  if (any(x$index <= 0, na.rm = TRUE)) {
    stop("cannot make an index with non-positive values")
  }
  invisible(x)
}

validate_contrib <- function(x) {
  if (ncol(x$contrib) != length(x$time)) {
    stop("number of time periods does not agree with number of contributions")
  }
  if (nrow(x$contrib) != length(x$levels)) {
    stop("number of levels does not agree with number of contributions")
  }
  invisible(x)
}

validate_piar_index <- function(x) {
  validate_levels(x)
  validate_time(x)
  validate_index_values(x)
  validate_contrib(x)
  x
}

#---- Undocumented methods ----
#' @importFrom utils str
#' @export
str.piar_index <- function(object, ...) {
  str(unclass(object), ...)
}

#' @export
summary.chainable_piar_index <- function(object, ...) {
  chkDots(...)
  cat(
    "Period-over-period price index",
    "for",
    length(object$levels),
    "levels over",
    length(object$time),
    "time periods",
    "\n"
  )
  invisible()
}

#' @export
summary.direct_piar_index <- function(object, ...) {
  chkDots(...)
  cat(
    "Fixed-base price index",
    "for",
    length(object$levels),
    "levels over",
    length(object$time),
    "time periods",
    "\n"
  )
  invisible()
}

#' @export
print.piar_index <- function(x, ...) {
  summary(x)
  print(as.matrix(x), ...)
  invisible(x)
}

#' Test if an object is a price index
#'
#' Test if an object is a index object or a subclass of an index object.
#'
#' @param x An object to test.
#'
#' @returns
#' `is_index()` returns `TRUE` if `x` inherits from [`piar_index`].
#'
#' `is_chainable_index()` returns `TRUE` if `x` inherits from
#' [`chainable_piar_index`].
#'
#' `is_direct_index()` returns `TRUE` if `x` inherits from
#' [`direct_piar_index`].
#'
#' @export
is_index <- function(x) {
  inherits(x, "piar_index")
}

#' @rdname is_index
#' @export
is_chainable_index <- function(x) {
  inherits(x, "chainable_piar_index")
}

#' @rdname is_index
#' @export
is_direct_index <- function(x) {
  inherits(x, "direct_piar_index")
}

#' Group generics
#' @noRd
#' @export
Math.piar_index <- function(x, ...) {
  stop(gettextf("'%s' not meaningful for index objects", .Generic))
}

#' @export
Ops.piar_index <- function(e1, e2) {
  boolean <- switch(
    .Generic,
    `<` = ,
    `>` = ,
    `==` = ,
    `!=` = ,
    `<=` = ,
    `>=` = TRUE,
    FALSE
  )
  if (!boolean) {
    stop(gettextf("'%s' not meaningful for index objects", .Generic))
  }
  if (is_index(e1)) {
    e1 <- as.matrix(e1)
  }
  if (is_index(e2)) {
    e2 <- as.matrix(e2)
  }
  NextMethod(.Generic)
}

#' @export
Summary.piar_index <- function(..., na.rm) {
  stop(gettextf("'%s' not meaningful for index objects", .Generic))
}
