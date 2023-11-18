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
#' Returns `TRUE` if `x` inherits from [`piar_aggregation_structure`]
#'
#' @export
is_aggregation_structure <- function(x) {
  inherits(x, "piar_aggregation_structure")
}

#' Get the weights for a price index aggregation structure
#'
#' Get and set the weights for a price index aggregation structure.
#'
#' @param object A price index aggregation structure, as made by
#' [aggregation_structure()].
#' @param ea_only Should weights be returned for only the elemental aggregates?
#' The default gives the weights for the entire aggregation structure.
#' @param na.rm Should missing values be removed when aggregating the
#' weights (i.e., when `ea_only = FALSE`)? By default, missing values are
#' not removed.
#' @param value A numeric vector of weights for the elemental aggregates of
#' `object`.
#' @param ... Further arguments passed to or used by methods.
#'
#' @returns
#' `weights()` returns a list with a named vector of weights for
#' each level in the aggregation structure. If `ea_only = TRUE` then the
#' return value is just a named vector of weights for the elemental aggregates.
#' The replacement method replaces these values without changing the
#' aggregation structure.
#'
#' @examples
#' # A simple aggregation structure
#' #            1
#' #      |-----+-----|
#' #      11          12
#' #  |---+---|       |
#' #  111     112     121
#' #  (1)     (3)     (4)
#'
#' aggregation_weights <- data.frame(
#'   level1 = c("1", "1", "1"),
#'   level2 = c("11", "11", "12"),
#'   ea     = c("111", "112", "121"),
#'   weight = c(1, 3, 4)
#' )
#'
#' pias <- as_aggregation_structure(aggregation_weights)
#'
#' # Extract the weights
#'
#' weights(pias)
#'
#' # ... or update them
#'
#' weights(pias) <- 1:3
#' weights(pias)
#'
#' @importFrom stats weights
#' @family aggregation structure methods
#' @export
weights.piar_aggregation_structure <- function(object, ea_only = FALSE,
                                               na.rm = FALSE, ...) {
  if (ea_only) {
    return(object$weights)
  }
  res <- vector("list", object$height)
  res[[1L]] <- object$weights

  for (i in seq_along(res)[-1L]) {
    res[[i]] <- vapply(
      object$child[[i - 1L]],
      \(z) sum(res[[i - 1L]][z], na.rm = na.rm),
      numeric(1L)
    )
  }
  rev(res)
}

#' @rdname weights.piar_aggregation_structure
#' @export
`weights<-` <- function(object, value) {
  UseMethod("weights<-")
}

#' @rdname weights.piar_aggregation_structure
#' @export
`weights<-.piar_aggregation_structure` <- function(object, value) {
  object$weights[] <- as.numeric(value)
  validate_piar_aggregation_structure(object)
}

#' @export
levels.piar_aggregation_structure <- function(x) {
  x$levels
}

#' @export
`levels<-.piar_aggregation_structure` <- function(x, value) {
  stop("cannot replace levels attribute for aggregation structure")
}
