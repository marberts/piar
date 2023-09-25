#---- Class generator ----
new_aggregate_index <- function(index,
                                contrib,
                                levels,
                                time,
                                r,
                                pias,
                                chainable) {
  res <- list(index = as.list(index),
              contrib = as.list(contrib),
              levels = as.character(levels),
              time = as.character(time),
              r = as.numeric(r),
              pias = as.list(pias))
  type <- if (chainable) "chainable_index" else "direct_index"
  structure(res, class = c("aggregate_index", type, "abstract_index"))
}

new_index <- function(index, contrib, levels, time, chainable) {
  res <- list(index = as.list(index),
              contrib = as.list(contrib),
              levels = as.character(levels),
              time = as.character(time))
  type <- if (chainable) "chainable_index" else "direct_index"
  structure(res, class = c(type, "abstract_index"))
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

validate_index <- function(x) {
  validate_levels(x$levels)
  validate_time(x$time)
  validate_index_values(x$index)
  validate_contrib(x$contrib)
  x
}

#---- Printing ----
str.abstract_index <- function(object, ...) {
  str(unclass(object), ...)
}

index_string <- function(x) {
  res <- c(aggregate_index = "aggregate",
           chainable_index = "period-over-period",
           direct_index = "fixed-base",
           abstract_index = "price index")[class(x)]
  res <- paste(res, collapse = " ")
  substr(res, 1, 1) <- toupper(substr(res, 1, 1))
  res
}

print.abstract_index <- function(x, ...) {
  cat(index_string(x), "for", length(x$levels), "levels over", length(x$time), 
      "time periods", "\n")
  print(as.matrix(x), ...)
  invisible(x)
}

#---- Getters and setters ----
levels.abstract_index <- function(x) {
  x$levels
}

`levels<-.abstract_index` <- function(x, value) {
  stop("cannot replace levels attribute")
}

time.abstract_index <- function(x, ...) {
  x$time
}

start.abstract_index <- function(x, ...) {
  x$time[1L]
}

end.abstract_index <- function(x, ...) {
  x$time[length(x$time)]
}
