#---- Class generator ----
new_piar_aggregation_structure <- function(child, parent, levels, eas,
                                           weights, height) {
  stopifnot(is.list(child))
  stopifnot(is.list(parent))
  stopifnot(is.character(levels))
  stopifnot(is.character(eas))
  stopifnot(is.double(weights))
  stopifnot(is.integer(height))
  res <- list(child = child, parent = parent, levels = levels,
              eas = eas, weights = weights, height = height)
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
    stop("invalid aggregation structure; the input is likely not a nested",
         "hierachy")
  }
  invisible(x)
}

validate_pias_weights <- function(x) {
  if (length(x$weights) != length(x$eas)) {
    stop("cannot make an aggregation structure with a different number of ",
         "weights and elemental aggregates")
  }
  invisible(x)
}

validate_piar_aggregation_structure <- function(x) {
  validate_pias_levels(x)
  validate_pias_structure(x)
  validate_pias_weights(x)
  x
}

#---- Printing ----
print.piar_aggregation_structure <- function(x, ...) {
  cat("Aggregation structure for", length(x$eas), "elemental aggregates with",
      x$height - 1L, "levels above the elemental aggregates", "\n")
  invisible(x)
}

str.piar_aggregation_structure <- function(object, ...) {
  str(unclass(object), ...)
}

#---- Getters and setters ----
weights.piar_aggregation_structure <- function(object, ea_only = FALSE,
                                               na.rm = FALSE, ...) {
  if (ea_only) {
    return(object$weights)
  }
  res <- vector("list", object$height)
  res[[1L]] <- object$weights
  
  for (i in seq_along(res)[-1L]) {
    res[[i]] <- vapply(object$child[[i - 1L]],
                       \(z) sum(res[[i - 1L]][z], na.rm = na.rm),
                       numeric(1L))
  }
  rev(res)
}

`weights<-` <- function(object, value) {
  UseMethod("weights<-")
}

`weights<-.piar_aggregation_structure` <- function(object, value) {
  object$weights[] <- as.numeric(value)
  validate_piar_aggregation_structure(object)
}

levels.piar_aggregation_structure <- function(x) {
  x$levels
}

`levels<-.piar_aggregation_structure` <- function(x, value) {
  stop("cannot replace levels attribute for aggregation structure")
}
