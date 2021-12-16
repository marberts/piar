#---- Calculate generalized-mean elemental indexes ----
elemental_index <- function(rel, ...) {
  UseMethod("elemental_index")
}

elemental_index.default <- function(rel, ...) {
  elemental_index(as.numeric(rel), ...)
}

elemental_index.numeric <- function(rel, period = gl(1, length(rel)), ea = gl(1, length(rel)),
                                    w, contrib = FALSE, chain = TRUE, na.rm = FALSE, r = 0, ...) {
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
    warning(gettext("some elements of 'rel or 'w' are less than or equal to 0"))
  }
  period <- as.factor(period)
  ea <- as.factor(ea) # ensures elemental aggregates are balanced
  periods <- levels(period)
  eas <- levels(ea)
  if (!(nlevels(period) && nlevels(ea))) {
    stop(gettext("cannot make an index with no periods or elemental aggregates"))
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
  # unweighted calculation
  if (missing(w)) {
    index <- Map(index_fun, rel, na.rm = na.rm)
    contributions <- if (contrib) Map(contrib_fun, rel)
  # weighted calculation
  } else {
    w <- Map(split, split(w, period), ea)
    index <- Map(index_fun, rel, w, na.rm = na.rm)
    contributions <- if (contrib) Map(contrib_fun, rel, w)
  }
  # mimic contributions structure instead of a NULL
  if (!contrib) {
    contributions <- rep(empty_contrib(eas), nlevels(period))
    names(contributions) <- periods
  }
  # return 'elemental' object
  res <- list(index = index,
              contrib = contributions,
              levels = eas,
              time = periods,
              has_contrib = contrib,
              chain = chain)
  structure(res, class = c("elem_ind", "ind"))
}

#---- Coerce to an elemental index ----
as_elemental_index <- function(x, ...) {
  UseMethod("as_elemental_index")
}

as_elemental_index.default <- function(x, ...) {
  as_elemental_index(as.matrix(x), ...)
}

as_elemental_index.matrix <- function(x, chain = TRUE, ...) {
  if (!(nrow(x) && ncol(x))) {
    stop(gettext("cannot make an elemental index with no periods or elemental aggregates"))
  }
  storage.mode(x) <- "numeric"
  if (is.null(rownames(x))) rownames(x) <- seq_len(nrow(x))
  if (is.null(colnames(x))) colnames(x) <- seq_len(ncol(x))
  levels <- rownames(x)
  periods <- colnames(x)
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

#---- Test ----
is_elemental_index <- function(x) inherits(x, "elem_ind")