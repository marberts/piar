.elemental_index <- function(x, period, ea, w1, w2, contrib, na.rm, index_fun, contrib_fun) {
  period <- as.factor(period)
  ea <- as.factor(ea) # as.factor() ensures eps is balanced
  if (contrib && is.null(names(x))) {
    names(x) <- sequential_names(x, ea, period)
  }
  # turn x and w into lists with components for each period
  # inside each component is a list for each ea containing relatives and weights
  eas <- split(ea, period)
  x <- Map(split, split(x, period), eas)
  w1 <- Map(split, split(w1, period), eas)
  if (!missing(w2)) w2 <- Map(split, split(w2, period), eas)
  # calculate elemental indexes
  index <- if (missing(w2)) {
    mapply(Vectorize(index_fun), x, w1, MoreArgs = list(na.rm = na.rm), SIMPLIFY = FALSE)
  } else {
    mapply(Vectorize(index_fun), x, w1, w2, MoreArgs = list(na.rm = na.rm), SIMPLIFY = FALSE)
  }
  # calculate quote contributions
  contributions <- if (contrib) {
    if (missing(w2)) {
      mapply(Vectorize(contrib_fun, SIMPLIFY = FALSE), x, w1, SIMPLIFY = FALSE)
    } else {
      mapply(Vectorize(contrib_fun, SIMPLIFY = FALSE), x, w1, w2, SIMPLIFY = FALSE)
    }
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


elemental_index <- function(x, period = rep(1L, length(x)), ea = rep(1L, length(x)),
                            w = rep(1, length(x)), contrib = FALSE, na.rm = FALSE, r = 0) {
  if (!same_length(x, period, ea, w)) {
    stop(gettext("'x', 'period', 'ea', and 'w' must be the same length"))
  }
  .elemental_index(x, period, ea, w, contrib = contrib, na.rm = na.rm, index_fun = generalized_mean(r), contrib_fun = contributions(r))
}

superlative_elemental_index <- function(x, period = rep(1L, length(x)), ea = rep(1L, length(x)),
                                        w1 = rep(1, length(x)), w2 = rep(1, length(x)), 
                                        contrib = FALSE, na.rm = FALSE, s = 2) {
  if (!same_length(x, period, ea, w1, w2)) {
    stop(gettext("'x', 'period', 'ea', 'w1', and 'w2' must be the same length"))
  }
  .elemental_index(x, period, ea, w1, w2, contrib = contrib, na.rm = na.rm, index_fun = nested_mean(0, c(s / 2, -s / 2)), contrib_fun = nested_contributions(0, c(s / 2, -s / 2)))
}
