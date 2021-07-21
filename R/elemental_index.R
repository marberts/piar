#---- Internal functions ----
.elemental_index <- function(x, period, ea, w1, w2, contrib, na.rm, index_fun, contrib_fun) {
  index_fun <- match.fun(index_fun)
  contrib_fun <- match.fun(contrib_fun)
  period <- as.factor(period)
  ea <- as.factor(ea) # as.factor() ensures eps is balanced
  if (contrib && is.null(names(x))) {
    names(x) <- sequential_names(ea, period)
  }
  # turn x and w into lists with components for each period
  # inside each component is a list for each ea containing relatives and weights
  eas <- split(ea, period)
  x <- Map(split, split(x, period), eas)
  if (!missing(w1)) w1 <- Map(split, split(w1, period), eas)
  if (!missing(w2)) w2 <- Map(split, split(w2, period), eas)
  # case 1 has no weights, case 2 has w1 and no w2, 
  # case 3 has no w1 and has w2, and case 4 has w1 and w2
  case <- 1 + (!missing(w1)) + 2 * (!missing(w2))
  # calculate elemental indexes
  vindex_fun <- Vectorize(index_fun)
  na.rm <- if (length(x)) na.rm # mapply complains about length-0 inputs
  index <- switch(case,
                  Map(vindex_fun, x, na.rm = na.rm),
                  Map(vindex_fun, x, w1, na.rm = na.rm),
                  Map(vindex_fun, x, w2 = w2, na.rm = na.rm),
                  Map(vindex_fun, x, w1, w2, na.rm = na.rm))
  # calculate quote contributions
  vcontrib_fun <- Vectorize(contrib_fun, SIMPLIFY = FALSE)
  contributions <- if (contrib) {
    switch(case,
           Map(vcontrib_fun, x),
           Map(vcontrib_fun, x, w1),
           Map(vcontrib_fun, x, w2 = w2),
           Map(vcontrib_fun, x, w1, w2))
  } else {
    Map(split, split(numeric(0), period), eas)
  }
  # return 'elemental' object
  res <- list(index = index,
              contributions = contributions,
              levels = levels(ea),
              periods = levels(period))
  structure(res, class = c("elemental", "index"))
}

#---- Exported functions ----
elemental_index <- function(x, period = rep(1L, length(x)), ea = rep(1L, length(x)),
                            w, contrib = FALSE, na.rm = FALSE, r = 0) {
  if (missing(w)) {
    if (!same_length(x, period, ea)) {
      stop(gettext("'x', 'period', and 'ea' must be the same length"))
    }
  } else {
    if (!same_length(x, period, ea, w)) {
      stop(gettext("'x', 'period', 'ea', 'w', must be the same length"))
    }
  }
  if (any_negative(x, if (!missing(w)) w)) {
    warning(gettext("some elements of 'x' or 'w' are less than or equal to 0"))
  }
  .elemental_index(x, period, ea, w, contrib = contrib, na.rm = na.rm, 
                   index_fun = generalized_mean(r), 
                   contrib_fun = contributions(r))
}

superlative_elemental_index <- function(x, period = rep(1L, length(x)), ea = rep(1L, length(x)),
                                        w1, w2, contrib = FALSE, na.rm = FALSE, s = 2) {
  if (missing(w1) && missing(w2)) {
    if (!same_length(x, period, ea)) {
      stop(gettext("'x', 'period', and 'ea' must be the same length"))
    }
  } else if (missing(w2)) {
    if (!same_length(x, period, ea, w1)) {
      stop(gettext("'x', 'period', 'ea', and 'w1' must be the same length"))
    }
  } else if (missing(w1)) {
    if (!same_length(x, period, ea, w2)) {
      stop(gettext("'x', 'period', 'ea', and 'w2' must be the same length"))
    }
  } else {
    if (!same_length(x, period, ea, w2)) {
      stop(gettext("'x', 'period', 'ea', 'w1', and 'w2' must be the same length"))
    }
  }
  if (any_negative(x, if (!missing(w1)) w1, if (!missing(w2)) w2)) {
    warning(gettext("some elements of 'x', 'w1', or 'w2' are less than or equal to 0"))
  }
  .elemental_index(x, period, ea, w1, w2, contrib = contrib, na.rm = na.rm, 
                   index_fun = nested_mean(0, c(s / 2, -s / 2)), 
                   contrib_fun = nested_contributions(0, c(s / 2, -s / 2)))
}
