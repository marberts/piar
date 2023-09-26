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
  if (!identical(x$time, names(x$index))) {
    stop("missing index values for each time period")
  }
  if (any(vapply(x$index, \(z) !identical(x$levels, names(z)), logical(1L)))) {
    stop("missing index values for each level")
  }
  invisible(x)
}

validate_contrib <- function(x) {
  if (!identical(x$time, names(x$contrib))) {
    stop("missing contributions for each time period")
  }
  if (any(vapply(x$contrib,
                 \(z) !identical(x$levels, names(z)), logical(1L)))) {
    stop("missing contributions for each level")
  }
  invisible(x)
}

validate_piar_index <- function(x) {
  validate_levels(x$levels)
  validate_time(x$time)
  validate_index_values(x$index)
  validate_contrib(x$contrib)
  x
}

validate_aggregate_piar_index <- function(x) {
  if (length(x$r) != 1) {
    stop("'r' must be of length 1")
  }
  # TODO: Should there be validation for the pias component?
  validate_piar_index(x)
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
  stop("cannot replace levels attribute")
}

time.piar_index <- function(x, ...) {
  x$time
}

start.piar_index <- function(x, ...) {
  x$time[1L]
}

end.piar_index <- function(x, ...) {
  x$time[length(x$time)]
}
