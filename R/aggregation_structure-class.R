#---- Class generator ----
new_piar_aggregation_structure <- function(child, parent, levels, weights) {
  stopifnot(is.list(child))
  stopifnot(is.list(parent))
  stopifnot(is.list(levels))
  stopifnot(is.double(weights))
  res <- list(
    child = child,
    parent = parent,
    levels = levels,
    weights = weights
  )
  structure(res, class = "piar_aggregation_structure")
}

piar_aggregation_structure <- function(child, parent, levels, weights) {
  levels <- lapply(as.list(levels), as.character)
  weights <- as.numeric(weights)
  validate_piar_aggregation_structure(
    new_piar_aggregation_structure(child, parent, levels, weights)
  )
}

#---- Validator ----
validate_pias_levels <- function(x) {
  lev <- unlist(x$levels, use.names = FALSE)
  if (missing_names(lev)) {
    stop(
      "cannot make an aggregation structure with missing or length-zero levels"
    )
  }
  if (anyDuplicated(lev)) {
    stop("cannot make an aggregation structure with duplicated levels")
  }
  invisible(x)
}

validate_pias_structure <- function(x) {
  height <- length(x$levels)
  if (
    height != length(x$child) + 1L ||
      height != length(x$parent) + 1L ||
      anyNA(x$child, recursive = TRUE) ||
      anyNA(x$parent, recursive = TRUE) ||
      any(vapply(x$child, \(x) any(lengths(x) == 0L), logical(1L)))
  ) {
    stop(
      "invalid aggregation structure; the input is likely not a nested ",
      "hierachy"
    )
  }
  invisible(x)
}

validate_pias_weights <- function(x) {
  if (length(x$weights) != length(last(x$levels))) {
    stop(
      "cannot make an aggregation structure with a different number of ",
      "weights and elementary aggregates"
    )
  }
  if (any(x$weights < 0, na.rm = TRUE)) {
    stop("cannot make an aggregation structure with negative weights")
  }
  invisible(x)
}

validate_piar_aggregation_structure <- function(x) {
  validate_pias_levels(x)
  validate_pias_structure(x)
  validate_pias_weights(x)
  x
}

#---- Undocumented methods ----
#' @export
summary.piar_aggregation_structure <- function(object, ...) {
  chkDots(...)
  cat(
    "Aggregation structure for",
    length(last(object$levels)),
    "elementary aggregates with",
    length(object$levels) - 1L,
    "levels above the elementary aggregates",
    "\n"
  )
  invisible()
}

#' @export
print.piar_aggregation_structure <- function(x, ...) {
  summary(x)
  print(as.data.frame(x, fix.empty.names = FALSE), ...)
  invisible(x)
}

#' @export
str.piar_aggregation_structure <- function(object, ...) {
  str(unclass(object), ...)
}

#' Test if an object is an aggregation structure
#'
#' Test if an object is a price index aggregation structure.
#'
#' @param x An object to test.
#'
#' @returns
#' Returns `TRUE` if `x` inherits from [`piar_aggregation_structure`].
#'
#' @export
is_aggregation_structure <- function(x) {
  inherits(x, "piar_aggregation_structure")
}
