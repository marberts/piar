.elemental_index <- function(x, period, ea, w, contrib, na.rm, index_fun, contrib_fun) {
  if (!same_length(x, period, ea, w)) {
    stop("'x', 'period', 'ea', and 'w' must be the same length")
  }
  period <- as.factor(period)
  ea <- as.factor(ea) # as.factor() ensures eps is balanced
  if (contrib && is.null(names(x))) {
    names(x) <- sequential_names(x, ea, period)
  }
  # turn x and w into lists with components for each period
  # inside each component is a list for each ea containing relatives and weights
  eas <- split(ea, period)
  x <- Map(split, split(x, period), eas)
  w <- Map(split, split(w, period), eas)
  # calculate elemental indexes
  index <- mapply(Vectorize(index_fun), x, w, MoreArgs = list(na.rm = na.rm), SIMPLIFY = FALSE)
  # calculate quote contributions
  contributions <- if (contrib) {
    mapply(Vectorize(contrib_fun, SIMPLIFY = FALSE), x, w, SIMPLIFY = FALSE)
  } else {
    Map(split, split(structure(numeric(0), names = character(0)), period), eas)
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
  .elemental_index(x, period, ea, w, contrib, na.rm, mean_generalized(r), contributions(r))
}
