#' Price index objects
#'
#' There are several classes to represent price indexes.
#' - All indexes inherit from the `piar_index` virtual class.
#' - Period-over-period indexes that can be chained over time inherit from
#' `chainable_piar_index`.
#' - Fixed-base indexes inherit from `direct_piar_index`.
#' - Aggregate price indexes that are the result of aggregating elemental
#' indexes with an aggregation structure further inherit from
#' `aggregate_piar_index`.
#'
#' The `piar_index` object is a list-S3 class with the following
#' components:
#' \describe{
#' \item{index}{A list with an entry for each period in `time` that gives
#' a vector of index values for each level in `levels`.}
#' \item{contrib}{A list with an entry for each period in `time`, which
#' itself contains a list with an entry for each level in `levels` with
#' a named vector that gives the additive contribution for each price relative.}
#' \item{levels}{A character vector giving the levels of the index.}
#' \item{time}{A character vector giving the time periods for the index.}
#' }
#'
#' The `chainable_piar_index` and `direct_piar_index` subclasses have
#' the same structure as the `piar_index` class, but differ in the methods
#' used to manipulate the indexes.
#'
#' The `aggregate_piar_index` class further subclasses either
#' `chainable_piar_index` or `direct_piar_index`, and adds the
#' following components:
#' \describe{
#' \item{r}{The order of the generalized mean used to aggregated the
#' index (usually 1).}
#' \item{pias}{A list containing the `child`, `parent`, `eas`, and `height`
#' components of the aggregation structured used to aggregate the index.}
#' }
#'
#' @name piar_index
#' @aliases piar_index chainable_piar_index direct_piar_index
#' aggregate_piar_index
#'
NULL

#---- Helpers ----
index_skeleton <- function(levels, time) {
  index <- rep.int(NA_real_, length(levels))
  rep.int(list(index), length(time))
}

empty_contrib <- function(x) {
  res <- rep.int(list(numeric(0L)), length(x))
  list(res)
}

contrib_skeleton <- function(levels, time) {
  rep.int(empty_contrib(levels), length(time))
}

has_contrib <- function(x) {
  Position(\(x) any(lengths(x) > 0L), x$contrib, nomatch = 0L) > 0L
}

#---- Class generator ----
new_aggregate_piar_index <- function(index, contrib, levels, time,
                                     r, pias, chainable) {
  stopifnot(is.list(index))
  stopifnot(is.list(contrib))
  stopifnot(is.character(levels))
  stopifnot(is.character(time))
  stopifnot(is.double(r))
  stopifnot(is.list(pias))
  res <- list(
    index = index, contrib = contrib, levels = levels,
    time = time, r = r, pias = pias
  )
  type <- if (chainable) "chainable_piar_index" else "direct_piar_index"
  structure(res, class = c("aggregate_piar_index", type, "piar_index"))
}

aggregate_piar_index <- function(index, contrib, levels, time,
                                 r, pias, chainable) {
  levels <- as.character(levels)
  time <- as.character(time)
  r <- as.numeric(r)
  validate_aggregate_piar_index(
    new_aggregate_piar_index(index, contrib, levels, time, r, pias, chainable)
  )
}

new_piar_index <- function(index, contrib, levels, time, chainable) {
  stopifnot(is.list(index))
  stopifnot(is.list(contrib))
  stopifnot(is.character(levels))
  stopifnot(is.character(time))
  res <- list(index = index, contrib = contrib, levels = levels, time = time)
  type <- if (chainable) "chainable_piar_index" else "direct_piar_index"
  structure(res, class = c(type, "piar_index"))
}

piar_index <- function(index, contrib, levels, time, chainable) {
  levels <- as.character(levels)
  time <- as.character(time)
  validate_piar_index(
    new_piar_index(index, contrib, levels, time, chainable)
  )
}

#---- Validator ----
validate_levels <- function(x) {
  if (length(x) == 0L) {
    stop("cannot make an index with no levels")
  }
  if (anyNA(x) || any(x == "")) {
    stop("cannot make an index with missing levels")
  }
  if (anyDuplicated(x)) {
    stop("cannot make an index with duplicate levels")
  }
  invisible(x)
}

validate_time <- function(x) {
  if (length(x) == 0L) {
    stop("cannot make an index with no time periods")
  }
  if (anyNA(x) || any(x == "")) {
    stop("cannot make an index with missing time periods")
  }
  if (anyDuplicated(x)) {
    stop("cannot make an index with duplicate time periods")
  }
  invisible(x)
}

validate_index_values <- function(x) {
  if (length(x$index) != length(x$time)) {
    stop("missing index values for each time period")
  }
  if (any(lengths(x$index) != length(x$levels))) {
    stop("missing index values for each level")
  }
  invisible(x)
}

validate_contrib <- function(x) {
  if (length(x$contrib) != length(x$time)) {
    stop("missing contributions for each time period")
  }
  if (any(lengths(x$contrib) != length(x$levels))) {
    stop("missing contributions for each level")
  }
  invisible(x)
}

validate_piar_index <- function(x) {
  validate_levels(x$levels)
  validate_time(x$time)
  validate_index_values(x)
  validate_contrib(x)
  x
}

validate_aggregate_piar_index <- function(x) {
  if (length(x$r) != 1) {
    stop("'r' must be of length 1")
  }
  validate_piar_index(x)
  validate_pias_structure(c(x$pias, levels = list(x$levels)))
  x
}

#' @importFrom utils str
#' @export
str.piar_index <- function(object, ...) {
  str(unclass(object), ...)
}

index_string <- function(x) {
  res <- c(
    aggregate_piar_index = "aggregate",
    chainable_piar_index = "period-over-period",
    direct_piar_index = "fixed-base",
    piar_index = "price index"
  )[class(x)]
  res <- paste(res, collapse = " ")
  substr(res, 1, 1) <- toupper(substr(res, 1, 1))
  res
}

#' @export
print.piar_index <- function(x, ...) {
  cat(
    index_string(x), "for", length(x$levels), "levels over", length(x$time),
    "time periods", "\n"
  )
  print(as.matrix(x), ...)
  invisible(x)
}

#' Get the levels for a price index
#'
#' Methods to get and set the levels for a price index.
#'
#' @param x A price index, as made by, e.g., [elemental_index()].
#' @param value A character vector, or something that can be coerced into one,
#' giving the replacement levels for `x`.
#'
#' @returns
#' `levels()` returns a character vector with the levels for a price index.
#'
#' The replacement method returns a copy of `x` with the levels in `value`.
#'
#' It's not generally possible to change the levels of an aggregate price
#' index, and in this case replacing the levels does not return an aggregate
#' index.
#'
#' @family index methods
#' @export
levels.piar_index <- function(x) {
  x$levels
}

#' @rdname levels.piar_index
#' @export
`levels<-.piar_index` <- function(x, value) {
  x$levels <- as.character(value)
  validate_piar_index(x)
}

#' @export
`levels<-.aggregate_piar_index` <- function(x, value) {
  value <- as.character(value)
  if (identical(value, x$levels)) {
    x
  } else {
    piar_index(x$index, x$contrib, value, x$time, is_chainable_index(x))
  }
}

#' Get the time periods for a price index
#'
#' Methods to get and set the time periods for a price index.
#'
#' @param x A price index, as made by, e.g., [elemental_index()].
#' @param value A character vector, or something that can be coerced into one,
#' giving the replacement time periods for `x`.
#' @param ... Further arguments passed to or used by methods.
#'
#' @returns
#' `time()` return a character vector with the time periods for a price index.
#' `start()` and `end()` return the first and last time period.
#'
#' The replacement method returns a copy of `x` with the time periods in
#' `value`.
#'
#' @importFrom stats time
#' @family index methods
#' @export
time.piar_index <- function(x, ...) {
  x$time
}

#' @rdname time.piar_index
#' @export
`time<-` <- function(x, value) {
  UseMethod("time<-")
}

#' @rdname time.piar_index
#' @export
`time<-.piar_index` <- function(x, value) {
  x$time <- as.character(value)
  validate_piar_index(x)
}

#' @rdname time.piar_index
#' @importFrom stats start
#' @export
start.piar_index <- function(x, ...) {
  x$time[1L]
}

#' @rdname time.piar_index
#' @importFrom stats end
#' @export
end.piar_index <- function(x, ...) {
  x$time[length(x$time)]
}

#' Test if an object is a price index
#'
#' Test if an object is a index object, or a subclass of an index object.
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
#' `is_aggregate_index()` returns `TRUE` if `x` inherits from
#' [`aggregate_piar_index`].
#'
#' @export
is_index <- function(x) {
  inherits(x, "piar_index")
}

#' @rdname is_index
#' @export
is_aggregate_index <- function(x) {
  inherits(x, "aggregate_piar_index")
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
  boolean <- switch(.Generic, `<` = , `>` = , `==` = , `!=` = , 
                    `<=` = , `>=` = TRUE, FALSE)
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
