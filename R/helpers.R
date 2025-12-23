#---- Useful functions ----
padded_extract <- function(x, i, pad) {
  pad <- as.character(pad)
  if (length(pad) != 1L) {
    stop("'pad' must be a length 1 character")
  }
  res <- x[seq_len(i)]
  res[seq.int(to = i, length.out = max(i - length(x), 0L))] <- pad
  res
}

missing_weights <- function(x) {
  is.na(x) | x == 0
}

missing_names <- function(x) {
  anyNA(x) || any(x == "")
}

last <- function(x) {
  x[[length(x)]]
}

drop_last <- function(x) {
  x[-length(x)]
}

match_eas <- function(pias, index) {
  match(last(pias$levels), index$levels)
}

same_hierarchy <- function(x, y) {
  identical(x[1:2], x[1:2])
}

#---- Replacing contributions ----
valid_contrib <- function(contrib) {
  if (is.null(names(contrib))) {
    products <- if (length(contrib) > 0L) {
      as.character(seq_along(contrib))
    }
  } else {
    products <- valid_product_names(names(contrib))
  }
  contrib <- as.numeric(contrib)
  names(contrib) <- products
  contrib
}

index2contrib <- function(index, levels, time) {
  contrib <- Map(stats::setNames, index - 1, levels)
  dim(contrib) <- c(length(levels), length(time))
  contrib
}

#---- Product names ----
which_duplicate_products <- function(x) {
  vapply(x, anyDuplicated, numeric(1L), incomparables = NA) > 0
}

duplicate_products <- function(x) {
  any(which_duplicate_products(x))
}

sequential_names <- function(...) {
  f <- interaction(...)
  unsplit(Map(seq_len, tabulate(f)), f)
}

valid_product_names <- function(x, period = NULL) {
  x <- as.character(x)
  if (missing_names(x)) {
    stop("each product must have a non-missing or length-zero name")
  }
  if (is.null(period)) {
    if (anyDuplicated(x)) {
      warning("product names are not unique")
      x <- make.unique(x)
    } else {
      x
    }
  } else {
    period <- as.factor(period)
    xs <- split(x, period)
    dups <- which_duplicate_products(xs)
    if (any(dups)) {
      warning("product names are not unique in each time period")
      xs[dups] <- lapply(xs[dups], make.unique)
      unsplit(xs, period)
    } else {
      x
    }
  }
}

#---- Validate vector inputs ----
different_length <- function(...) {
  res <- lengths(Filter(Negate(is.null), list(...)))
  any(res != res[1L])
}

formula_vars <- function(formula, x, n = 2L) {
  if (length(formula) != 3L) {
    stop("'formula' must have a left-hand and right-hand side")
  }
  fterms <- stats::terms(formula, data = x)
  x <- eval(attr(fterms, "variables"), x, environment(formula))
  if (length(x) != n + 1L) {
    stop(gettextf("right-hand side of 'formula' must have exactly %s terms", n))
  }
  x
}

#---- Subscript indexes ----
dim_indices <- function(x, i) {
  if (missing(i)) {
    return(seq_along(x))
  }
  if (anyNA(i)) {
    stop("cannot subscript with missing values")
  }
  if (is.character(i)) {
    res <- match(i, x)
  } else {
    if (is.logical(i)) {
      if (length(i) > length(x)) {
        stop("logical subscript too long")
      } else if (length(x) %% length(i) != 0) {
        warning("logical subscript is not a multiple of dimension length")
      }
    }
    res <- match(x[i], x)
  }
  if (anyNA(res)) {
    stop("subscript out of bounds")
  }
  res
}

match_dim <- function(what = c("time", "levels")) {
  what <- match.arg(what)
  dim <- switch(what, time = "time period", levels = "index level")
  function(x, index, several = FALSE) {
    if (!several && length(x) != 1L) {
      stop(gettextf("must supply exactly one %s", dim))
    } else if (several && length(x) == 0L) {
      stop(gettextf("must supply at least one %s", dim))
    }
    i <- match(x, index[[what]])
    no_match <- is.na(i)
    if (any(no_match)) {
      stop(gettextf("'%s' is not a %s", x[no_match][1L], dim))
    }
    i
  }
}

match_levels <- match_dim("levels")

match_time <- match_dim("time")

#---- Generate index ----
index_skeleton <- function(levels, time) {
  matrix(NA_real_, length(levels), length(time))
}

empty_contrib <- function(x) {
  res <- rep.int(list(numeric(0L)), length(x))
  list(res)
}

contrib_skeleton <- function(levels, time) {
  matrix(list(numeric(0L)), length(levels), length(time))
}

has_contrib <- function(x) {
  Position(\(x) any(lengths(x) > 0L), x$contrib, nomatch = 0L) > 0L
}

# Backport Reduce and %||%
# TODO: Remove once min R version gets bumped.
if (getRversion() < "4.4.0") {
  Reduce <- function(
    f,
    x,
    init,
    right = FALSE,
    accumulate = FALSE,
    simplify = TRUE
  ) {
    mis <- missing(init)
    len <- length(x)
    if (len == 0L) {
      return(if (mis) NULL else init)
    }
    f <- match.fun(f)
    if (!is.vector(x) || is.object(x)) {
      x <- as.list(x)
    }
    ind <- seq_len(len)
    if (mis) {
      if (right) {
        init <- x[[len]]
        ind <- ind[-len]
      } else {
        init <- x[[1L]]
        ind <- ind[-1L]
      }
    }
    if (!accumulate) {
      if (right) {
        for (i in rev(ind)) {
          init <- forceAndCall(
            2,
            f,
            x[[i]],
            init
          )
        }
      } else {
        for (i in ind) {
          init <- forceAndCall(2, f, init, x[[i]])
        }
      }
      init
    } else {
      len <- length(ind) + 1L
      out <- vector("list", len)
      if (mis) {
        if (right) {
          out[[len]] <- init
          for (i in rev(ind)) {
            init <- forceAndCall(2, f, x[[i]], init)
            out[[i]] <- init
          }
        } else {
          out[[1L]] <- init
          for (i in ind) {
            init <- forceAndCall(2, f, init, x[[i]])
            out[[i]] <- init
          }
        }
      } else {
        if (right) {
          out[[len]] <- init
          for (i in rev(ind)) {
            init <- forceAndCall(2, f, x[[i]], init)
            out[[i]] <- init
          }
        } else {
          for (i in ind) {
            out[[i]] <- init
            init <- forceAndCall(2, f, init, x[[i]])
          }
          out[[len]] <- init
        }
      }
      if (all(lengths(out) == 1L) && simplify) {
        out <- unlist(out, recursive = FALSE)
      }
      out
    }
  }

  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }
}
