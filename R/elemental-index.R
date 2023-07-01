#---- Calculate generalized-mean elemental indexes ----
elemental_index <- function(rel, ...) {
  UseMethod("elemental_index")
}

elemental_index.default <- function(rel, ...) {
  elemental_index(as.numeric(rel), ...)
}

elemental_index.numeric <-
  function(rel,
           period = gl(1, length(rel)), ea = gl(1, length(rel)), w = NULL,
           contrib = FALSE, chainable = TRUE, na.rm = FALSE, r = 0, ...) {
  if (is.null(w)) {
    if (different_length(rel, period, ea)) {
      stop("'rel', 'period', and 'ea' must be the same length")
    }
  } else {
    if (different_length(rel, period, ea, w)) {
      stop("'rel', 'period', 'ea', 'w', must be the same length")
    }
  }
  if (any_negative(rel, w)) {
    warning("some elements of 'rel or 'w' are less than or equal to 0")
  }

  contrib <- as_TorF(contrib)
  chainable <- as_TorF(chainable)
  period <- as.factor(period)
  ea <- as.factor(ea) # ensures elemental aggregates are balanced
  periods <- levels(period)
  eas <- levels(ea)

  if (nlevels(period) == 0L || nlevels(ea) == 0L) {
    stop("cannot make an index with no periods or elemental aggregates")
  }
  if (contrib && is.null(names(rel))) {
    names(rel) <- sequential_names(ea, period)
  }
  # splitting 'rel' into a nested list by period then ea is the same as
  # using interaction(), but makes it easier to get the results as
  # a list
  ea <- split(ea, period)
  rel <- Map(split, split(rel, period), ea)
  # vectorize index and contribution functions to map over the
  # nested list of relatives
  index_fun <- Vectorize(generalized_mean(r))
  contrib_fun <- Vectorize(contributions(r), SIMPLIFY = FALSE)

  w <- if (is.null(w)) {
    list(list(NULL))
  } else {
    Map(split, split(w, period), ea)
  }

  index <- Map(index_fun, rel, w, na.rm = na.rm)
  contributions <- if (contrib) {
    Map(contrib_fun, rel, w)
  }
  # mimic contributions structure instead of a NULL
  if (!contrib) {
    contributions <- rep.int(empty_contrib(eas), nlevels(period))
    names(contributions) <- periods
  }
  # return 'ind' object
  res <- list(index = index,
              contrib = contributions,
              levels = eas,
              time = periods,
              has_contrib = contrib,
              chainable = chainable)
  structure(res, class = "ind")
}
