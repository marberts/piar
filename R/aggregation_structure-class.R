#---- Class generator ----
new_piar_aggregation_structure <- function(child, parent, levels, eas,
                                           weights, height) {
  stopifnot(is.list(child))
  stopifnot(is.list(parent))
  stopifnot(is.character(levels))
  stopifnot(is.character(eas))
  stopifnot(is.double(weights))
  stopifnot(is.integer(height))
  res <- list(
    child = child, parent = parent, levels = levels,
    eas = eas, weights = weights, height = height
  )
  structure(res, class = "piar_aggregation_structure")
}

piar_aggregation_structure <- function(child, parent, levels, eas,
                                       weights, height) {
  levels <- as.character(levels)
  eas <- as.character(eas)
  weights <- as.numeric(weights)
  names(weights) <- eas
  height <- as.integer(height)
  validate_piar_aggregation_structure(
    new_piar_aggregation_structure(child, parent, levels, eas, weights, height)
  )
}

#---- Validator ----
validate_pias_levels <- function(x) {
  if (anyNA(x$levels) || any(x$levels == "")) {
    stop("cannot make an aggregation structure with missing levels")
  }
  if (anyDuplicated(x$levels)) {
    stop("cannot make an aggregation structure with duplicated levels")
  }
  invisible(x)
}

validate_pias_structure <- function(x) {
  eas <- seq.int(to = length(x$levels), length.out = length(x$eas))
  if (!identical(x$eas, x$levels[eas]) ||
    x$height != length(x$child) + 1L ||
    x$height != length(x$parent) + 1L ||
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
  if (length(x$weights) != length(x$eas)) {
    stop(
      "cannot make an aggregation structure with a different number of ",
      "weights and elemental aggregates"
    )
  }
  invisible(x)
}

validate_piar_aggregation_structure <- function(x) {
  validate_pias_levels(x)
  validate_pias_structure(x)
  validate_pias_weights(x)
  x
}

#' @export
print.piar_aggregation_structure <- function(x, ...) {
  cat(
    "Aggregation structure for", length(x$eas), "elemental aggregates with",
    x$height - 1L, "levels above the elemental aggregates", "\n"
  )
  print(as.data.frame(x), ...)
  invisible(x)
}

#' @export
str.piar_aggregation_structure <- function(object, ...) {
  str(unclass(object), ...)
}

#' Test if an object is a price index aggregation structure
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
