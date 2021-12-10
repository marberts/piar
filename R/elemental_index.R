#---- Internal functions ----
.elemental_index <- function(rel, period, ea, w1, w2, contrib, chain, na.rm, index_fun, contrib_fun) {
  index_fun <- match.fun(index_fun)
  contrib_fun <- match.fun(contrib_fun)
  period <- as.factor(period)
  ea <- as.factor(ea) # as.factor() ensures eps is balanced
  if (contrib && is.null(names(rel))) {
    names(rel) <- sequential_names(ea, period)
  }
  # turn x and w into lists with components for each period
  # inside each component is a list for each ea containing relatives and weights
  eas <- split(ea, period)
  rel <- Map(split, split(rel, period), eas)
  if (!missing(w1)) w1 <- Map(split, split(w1, period), eas)
  if (!missing(w2)) w2 <- Map(split, split(w2, period), eas)
  # case 1 has no weights, case 2 has w1 and no w2, 
  # case 3 has no w1 and has w2, and case 4 has w1 and w2
  case <- 1 + (!missing(w1)) + 2 * (!missing(w2))
  # calculate elemental indexes
  vindex_fun <- Vectorize(index_fun)
  na.rm <- if (length(rel)) na.rm # mapply complains about length-0 inputs
  index <- switch(case,
                  Map(vindex_fun, rel, na.rm = na.rm),
                  Map(vindex_fun, rel, w1, na.rm = na.rm),
                  Map(vindex_fun, rel, w2 = w2, na.rm = na.rm),
                  Map(vindex_fun, rel, w1, w2, na.rm = na.rm))
  # calculate quote contributions
  vcontrib_fun <- Vectorize(contrib_fun, SIMPLIFY = FALSE)
  contributions <- if (contrib) {
    switch(case,
           Map(vcontrib_fun, rel),
           Map(vcontrib_fun, rel, w1),
           Map(vcontrib_fun, rel, w2 = w2),
           Map(vcontrib_fun, rel, w1, w2))
  } else {
    Map(split, split(numeric(0), period), eas)
  }
  # return 'elemental' object
  res <- list(index = index,
              contrib = contributions,
              levels = levels(ea),
              time = levels(period),
              has_contrib = contrib,
              chain = chain)
  structure(res, class = c("elem_ind", "ind"))
}

#---- Calculate generalized-mean elemental indexes ----
elemental_index <- function(rel, period = rep(1L, length(rel)), ea = rep(1L, length(rel)),
                            w, contrib = FALSE, chain = TRUE, na.rm = FALSE, r = 0) {
  if (missing(w)) {
    if (different_length(rel, period, ea)) {
      stop(gettext("'rel', 'period', and 'ea' must be the same length"))
    }
  } else {
    if (different_length(rel, period, ea, w)) {
      stop(gettext("'rel', 'period', 'ea', 'w', must be the same length"))
    }
  }
  if (any_negative(rel, if (!missing(w)) w)) {
    warning(gettext("some elements of 'rel' or 'w' are less than or equal to 0"))
  }
  .elemental_index(rel, period, ea, w, contrib = contrib, na.rm = na.rm, chain = chain,
                   index_fun = generalized_mean(r), 
                   contrib_fun = contributions(r))
}

#---- Calculate superlative elemental indexes ----
superlative_elemental_index <- function(rel, period = rep(1L, length(rel)), ea = rep(1L, length(rel)),
                                        w1, w2, contrib = FALSE, chain = TRUE, na.rm = FALSE, s = 2) {
  if (missing(w1) && missing(w2)) {
    if (different_length(rel, period, ea)) {
      stop(gettext("'rel', 'period', and 'ea' must be the same length"))
    }
  } else if (missing(w2)) {
    if (different_length(rel, period, ea, w1)) {
      stop(gettext("'rel', 'period', 'ea', and 'w1' must be the same length"))
    }
  } else if (missing(w1)) {
    if (different_length(rel, period, ea, w2)) {
      stop(gettext("'rel', 'period', 'ea', and 'w2' must be the same length"))
    }
  } else {
    if (different_length(rel, period, ea, w2)) {
      stop(gettext("'rel', 'period', 'ea', 'w1', and 'w2' must be the same length"))
    }
  }
  if (any_negative(rel, if (!missing(w1)) w1, if (!missing(w2)) w2)) {
    warning(gettext("some elements of 'rel', 'w1', or 'w2' are less than or equal to 0"))
  }
  .elemental_index(rel, period, ea, w1, w2, contrib = contrib, na.rm = na.rm, chain = chain,
                   index_fun = nested_mean(0, c(s / 2, -s / 2)), 
                   contrib_fun = nested_contributions(0, c(s / 2, -s / 2)))
}

#---- Coerce to an elemental index ----
as_elemental_index <- function(x, ...) {
  UseMethod("as_elemental_index")
}

as_elemental_index.default <- function(x, ...) {
  as_elemental_index(as.matrix(x), ...)
}

as_elemental_index.matrix <- function(x, chain = TRUE, ...) {
  storage.mode(x) <- "numeric"
  if (is.null(rownames(x))) rownames(x) <- seq_len(nrow(x))
  if (is.null(colnames(x))) colnames(x) <- seq_len(ncol(x))
  levels <- as.character(rownames(x)) # as.character is for matrices without rows
  periods <- as.character(colnames(x)) # same for columns
  if (anyDuplicated(levels) || anyDuplicated(periods)) {
    stop(gettext("'x' cannot have duplicated row or column names"))
  }
  res <- list(index = NULL, contrib = NULL, levels = levels, 
              time = periods, has_contrib = FALSE, chain = chain)
  res$index <- res$contrib <- 
    structure(vector("list", ncol(x)), names = periods)
  res$contrib[] <- empty_contrib(levels)
  for (t in seq_along(periods)) {
    # EA names are not kept for matrices with 1 row
    res$index[[t]] <- structure(x[, t], names = rownames(x))
  }
  structure(res, class = c("elem_ind", "ind"))
}

as_elemental_index.agg_ind <- function(x, ...) {
  eas <- x$pias$eas
  index <- lapply(x$index, `[`, eas)
  contrib <- lapply(x$contrib, `[`, eas)
  structure(list(index = index, 
                 contrib = contrib, 
                 levels = eas, 
                 time = x$time,
                 has_contrib = x$has_contrib, 
                 chain = x$chain), 
            class = c("elem_ind", "ind"))
}