new_index <- function(index, contrib, levels, time, chainable) {
  res <- list(index = index, contrib = contrib, levels = levels, time = time)
  type <- if (chainable) "chainable_index" else "direct_index"
  structure(res, class = c(type, "index"))
}

#---- Calculate generalized-mean elemental indexes ----
elemental_index <- function(x, ...) {
  UseMethod("elemental_index")
}

elemental_index.default <- function(x, ...) {
  elemental_index(as.numeric(x), ...)
}

elemental_index.numeric <- function(x,
                                    period = gl(1, length(x)),
                                    ea = gl(1, length(x)),
                                    w = NULL,
                                    contrib = FALSE,
                                    chainable = TRUE,
                                    na.rm = FALSE,
                                    r = 0,
                                    ...) {
  if (different_length(x, period, ea, w)) {
    stop("input vectors must be the same length")
  }
  if (any(x <= 0, na.rm = TRUE) || any(w <= 0, na.rm = TRUE)) {
    warning("some elements of 'x or 'w' are less than or equal to 0")
  }

  period <- as.factor(period)
  ea <- as.factor(ea) # ensures elemental aggregates are balanced
  time <- levels(period)
  levels <- levels(ea)

  if (length(time) == 0L || length(levels) == 0L) {
    stop("cannot make an index with no periods or elemental aggregates")
  }
  if (anyNA(time) || anyNA(levels)) {
    stop("cannot make an index with missing periods or elemental aggregates")
  }
  if (contrib) {
    if (is.null(names(x))) {
      names(x) <- sequential_names(ea, period)
    } else {
      names(x) <- valid_product_names(names(x), period, ea)
    }
  }
  # splitting 'x' into a nested list by period then ea is the same as
  # using interaction(), but makes it easier to get the results as
  # a list
  ea <- split(ea, period)
  x <- Map(split, split(x, period), ea)
  if (is.null(w)) {
    w <- list(list(NULL)) 
  } else {
    w <- Map(split, split(as.numeric(w), period), ea)
  }

  index_fun <- Vectorize(generalized_mean(r))
  contrib_fun <- Vectorize(contributions(r), SIMPLIFY = FALSE)

  index <- Map(index_fun, x, w, na.rm = na.rm)
  if (contrib) {
    contributions <- Map(contrib_fun, x, w)
  } else {
    # mimic contributions structure instead of a NULL
    contributions <- rep.int(empty_contrib(levels), length(time))
    names(contributions) <- time
  }

  new_index(index, contributions, levels, time, chainable)
}
