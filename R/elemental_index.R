#---- Internal functions ----
.elemental <- function(x, period, ea, w1, w2, f) {
  fun <- match.fun(f)
  # turn x and w into lists with components for each period
  # inside each component is a list for each ea containing relatives and weights
  ea <- split(ea, period)
  x <- Map(split, split(x, period), ea)
  if (!missing(w1)) w1 <- Map(split, split(w1, period), ea)
  if (!missing(w2)) w2 <- Map(split, split(w2, period), ea)
  # case 1 has no weights, case 2 has w1 and no w2, 
  # case 3 has no w1 and has w2, and case 4 has w1 and w2
  switch(1 + (!missing(w1)) + 2 * (!missing(w2)),
         Map(f, x),
         Map(f, x, w1),
         Map(f, x, w2 = w2),
         Map(f, x, w1, w2))
}

.elemental_index <- function(x, period, ea, w1, w2, f) {
  f <- Vectorize(match.fun(f))
  period <- as.factor(period)
  ea <- as.factor(ea) # as.factor() ensures eps is balanced
  res <- list(index = .elemental(x, period, ea, w1, w2, f),
              levels = levels(ea),
              periods = levels(period))
  structure(res, class = c("elem_index", "index"))
}

.elemental_contrib <- function(x, period, ea, w1, w2, f) {
  f <- Vectorize(match.fun(f), SIMPLIFY = FALSE)
  period <- as.factor(period)
  ea <- as.factor(ea) # as.factor() ensures eps is balanced
  # make product names if there are none
  if (is.null(names(x))) {
    names(x) <- sequential_names(ea, period)
  }
  res <- list(contributions = .elemental(x, period, ea, w1, w2, f),
              levels = levels(ea),
              period = levels(period))
  structure(res, class = c("elem_contrib", "contrib"))
}

check_generalized_index <- function(x, period, ea, w) {
  if (missing(w)) {
    if (different_length(x, period, ea)) {
      stop(gettext("'x', 'period', and 'ea' must be the same length"))
    }
  } else {
    if (different_length(x, period, ea, w)) {
      stop(gettext("'x', 'period', 'ea', 'w', must be the same length"))
    }
  }
  if (any_negative(x, if (!missing(w)) w)) {
    warning(gettext("some elements of 'x' or 'w' are less than or equal to 0"))
  }
}

check_superlative_index <- function(x, period, ea, w1, w2) {
  if (missing(w1) && missing(w2)) {
    if (different_length(x, period, ea)) {
      stop(gettext("'x', 'period', and 'ea' must be the same length"))
    }
  } else if (missing(w2)) {
    if (different_length(x, period, ea, w1)) {
      stop(gettext("'x', 'period', 'ea', and 'w1' must be the same length"))
    }
  } else if (missing(w1)) {
    if (different_length(x, period, ea, w2)) {
      stop(gettext("'x', 'period', 'ea', and 'w2' must be the same length"))
    }
  } else {
    if (different_length(x, period, ea, w2)) {
      stop(gettext("'x', 'period', 'ea', 'w1', and 'w2' must be the same length"))
    }
  }
  if (any_negative(x, if (!missing(w1)) w1, if (!missing(w2)) w2)) {
    warning(gettext("some elements of 'x', 'w1', or 'w2' are less than or equal to 0"))
  }
}

#---- Exported functions ----
elemental_index <- function(x, period = rep(1L, length(x)), ea = rep(1L, length(x)),
                            w, na.rm = FALSE, r = 0) {
  check_generalized_index(x, period, ea, w)
  f <- generalized_mean(r)
  .elemental_index(x, period, ea, w, f = function(x, w) f(x, w, na.rm))
}

elemental_contributions <- function(x, period = rep(1L, length(x)), ea = rep(1L, length(x)),
                            w, r = 0) {
  check_generalized_index(x, period, ea, w)
  .elemental_contrib(x, period, ea, w, f = contributions(r))
}

superlative_elemental_index <- function(x, period = rep(1L, length(x)), ea = rep(1L, length(x)),
                                        w1, w2, na.rm = FALSE, s = 2) {
  check_superlative_index(x, period, ea, w1, w2)
  f <- nested_mean(0, c(s / 2, -s / 2))
  .elemental_index(x, period, ea, w1, w2, function(x, w1, w2) f(x, w1, w2, na.rm))
}

superlative_elemental_contributions <- function(x, period = rep(1L, length(x)), ea = rep(1L, length(x)),
                                        w1, w2, s = 2) {
  check_superlative_index(x, period, ea, w1, w2)
  .elemental_contrib(x, period, ea, w1, w2, nested_contributions(0, c(s / 2, -s / 2)))
}

