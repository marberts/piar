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
  res <- list(index = index, contrib = contrib, levels = levels,
              time = time, r = r, pias = pias)
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



#' Price index objects
#' 
#' There are several classes to represent price indexes. \itemize{ \item All
#' indexes inherit from the `piar_index` virtual class. \item
#' Period-over-period indexes that can be chained over time inherit from
#' `chainable_piar_index`. \item Fixed-base indexes inherit from
#' `direct_piar_index`.  \item Aggregate price indexes that are the result
#' of aggregating elemental indexes with an aggregation structure further
#' inherit from `aggregate_piar_index`. }
#' 
#' The `piar_index` object is a list-S3 class with the following
#' components: \describe{ \item{index}{A list with an entry for each period in
#' `time` that gives a vector of index values for each level in
#' `levels`.} \item{contrib}{A list with an entry for each period in
#' `time`, which itself contains a list with an entry for each level in
#' `levels` with a named vector that gives the additive contribution for
#' each price relative.} \item{levels}{A character vector giving the levels of
#' the index.} \item{time}{A character vector giving the time periods for the
#' index.} }
#' 
#' The `chainable_piar_index` and `direct_piar_index` subclasses have
#' the same structure as the `piar_index` class, but differ in the methods
#' used to manipulate the indexes.
#' 
#' The `aggregate_piar_index` class further subclasses either
#' `chainable_piar_index` or `direct_piar_index`, and adds the
#' following components: \describe{ \item{r}{The order of the generalized mean
#' used to aggregated the index (usually 1).} \item{pias}{A list containing the
#' `child`, `parent`, `eas`, and `height` components of the
#' aggregation structured used to aggregate the index.} }
#' 
#' @aliases piar_index chainable_piar_index direct_piar_index
#' aggregate_piar_index is_index is_aggregate_index is_chainable_index
#' is_direct_index
#' @param x An object to test.
#' @return `is_index()` returns `TRUE` if `x` inherits from
#' `piar_index`.
#' 
#' `is_chainable_index()` returns `TRUE` if `x` inherits from
#' `chainable_piar_index`.
#' 
#' `is_direct_index()` returns `TRUE` if `x` inherits from
#' `direct_piar_index`.
#' 
#' `is_aggregate_index()` returns `TRUE` if `x` inherits from
#' `aggregate_piar_index`.
#' @examples
#' 
#' prices <- data.frame(
#'   rel = 1:8,
#'   period = rep(1:2, each = 4),
#'   ea = rep(letters[1:2], 4)
#' )
#' 
#' pias <- aggregation_structure(
#'   list(c("top", "top", "top"), c("a", "b", "c")), 1:3
#' )
#' 
#' # Calculate period-over-period Jevons elemental indexes
#' 
#' epr <- with(prices, elemental_index(rel, period, ea))
#' 
#' # Aggregate as an arithmetic index
#' 
#' index <- aggregate(epr, pias, na.rm = TRUE)
#' 
#' is_index(epr)
#' is_chainable_index(epr)
#' is_direct_index(epr)
#' is_aggregate_index(epr)
#' 
#' is_index(index)
#' is_chainable_index(index)
#' is_direct_index(index)
#' is_aggregate_index(index)
#' 
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

#---- Printing ----
str.piar_index <- function(object, ...) {
  str(unclass(object), ...)
}

index_string <- function(x) {
  res <- c(aggregate_piar_index = "aggregate",
           chainable_piar_index = "period-over-period",
           direct_piar_index = "fixed-base",
           piar_index = "price index")[class(x)]
  res <- paste(res, collapse = " ")
  substr(res, 1, 1) <- toupper(substr(res, 1, 1))
  res
}

print.piar_index <- function(x, ...) {
  cat(index_string(x), "for", length(x$levels), "levels over", length(x$time), 
      "time periods", "\n")
  print(as.matrix(x), ...)
  invisible(x)
}

#---- Getters and setters ----
levels.piar_index <- function(x) {
  x$levels
}

`levels<-.piar_index` <- function(x, value) {
  x$levels <- as.character(value)
  validate_piar_index(x)
}

`levels<-.aggregate_piar_index` <- function(x, value) {
  value <- as.character(value)
  piar_index(x$index, x$contrib, value, x$time, is_chainable_index(x))
}

time.piar_index <- function(x, ...) {
  x$time
}

`time<-` <- function(x, value) {
  UseMethod("time<-")
}

`time<-.piar_index` <- function(x, value) {
  x$time <- as.character(value)
  validate_piar_index(x)
}

start.piar_index <- function(x, ...) {
  x$time[1L]
}

end.piar_index <- function(x, ...) {
  x$time[length(x$time)]
}
