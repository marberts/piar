pindex <- function(index, contrib, levels, time, chainable) {
  res <- list(index = index, contrib = contrib, levels = levels, time = time)
  type <- if (chainable) {
    "chainable_pindex"
  } else {
    "direct_pindex"
  }
  structure(res, class = c(type, "pindex"))
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
  if (is.null(w)) {
    if (different_length(x, period, ea)) {
      stop("'x', 'period', and 'ea' must be the same length")
    }
  } else {
    if (different_length(x, period, ea, w)) {
      stop("'x', 'period', 'ea', 'w', must be the same length")
    }
  }
  if (any_negative(x, w)) {
    warning("some elements of 'x or 'w' are less than or equal to 0")
  }

  period <- as.factor(period)
  ea <- as.factor(ea) # ensures elemental aggregates are balanced
  periods <- levels(period)
  eas <- levels(ea)

  if (nlevels(period) == 0L || nlevels(ea) == 0L) {
    stop("cannot make an index with no periods or elemental aggregates")
  }
  if (contrib && is.null(names(x))) {
    names(x) <- sequential_names(ea, period)
  }
  # splitting 'x' into a nested list by period then ea is the same as
  # using interaction(), but makes it easier to get the results as
  # a list
  ea <- split(ea, period)
  x <- Map(split, split(x, period), ea)
  w <- if (is.null(w)) {
    list(list(NULL))
  } else {
    Map(split, split(w, period), ea)
  }

  # vectorize index and contribution functions to map over the
  # nested list of relatives
  index_fun <- Vectorize(generalized_mean(r))
  contrib_fun <- Vectorize(contributions(r), SIMPLIFY = FALSE)

  index <- Map(index_fun, x, w, na.rm = na.rm)
  if (contrib) {
    contributions <- Map(contrib_fun, x, w)
  } else {
    # mimic contributions structure instead of a NULL
    contributions <- rep.int(empty_contrib(eas), nlevels(period))
    names(contributions) <- periods
  }

  pindex(index, contributions, eas, periods, chainable)
}
