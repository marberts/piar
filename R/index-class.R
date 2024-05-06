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
#' \item{index}{A list with an entry for each period in `time` that gives
#' a vector of index values for each level in `levels`.}
#' \item{contrib}{A list with an entry for each period in `time`, which
#' itself contains a list with an entry for each level in `levels` with
#' a named vector that gives the percent-change contribution for each price
#' relative.}
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

match_dim <- function(what) {
  what <- as.character(what)
  function(x, dim, several = FALSE) {
    if (!several && length(x) != 1L) {
      stop(gettextf("must supply exactly one %s", what))
    } else if (several && length(x) == 0L) {
      stop(gettextf("must supply at least one %s", what))
    }
    i <- match(x, dim)
    no_match <- is.na(i)
    if (any(no_match)) {
      stop(gettextf("'%s' is not a %s", x[no_match][1L], what))
    }
    i
  }
}

match_levels <- match_dim("index level")

match_time <- match_dim("time period")

#---- Class generator ----
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
    stop("number of time periods does not agree with number of index values")
  }
  if (any(lengths(x$index) != length(x$levels))) {
    stop("number of levels does not agree with number of index values")
  }
  invisible(x)
}

validate_contrib <- function(x) {
  if (length(x$contrib) != length(x$time)) {
    stop("number of time periods does not agree with number of contributions")
  }
  if (any(lengths(x$contrib) != length(x$levels))) {
    stop("number of levels does not agree with number of contributions")
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

#' @importFrom utils str
#' @export
str.piar_index <- function(object, ...) {
  str(unclass(object), ...)
}

#' @export
summary.chainable_piar_index <- function(object, ...) {
  cat(
    "Period-over-period price index", "for", length(object$levels), "levels over",
    length(object$time), "time periods", "\n"
  )
  invisible()
}

#' @export
summary.direct_piar_index <- function(object, ...) {
  cat(
    "Fixed-base price index", "for", length(object$levels), "levels over",
    length(object$time), "time periods", "\n"
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
