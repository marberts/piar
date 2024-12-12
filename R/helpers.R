#---- Replacing contributions ----
near <- function(x, y, tol = .Machine$double.eps^0.5) {
  abs(x - y) < tol
}

valid_replacement_contrib <- function(x, value) {
  if (length(value) == 0L) {
    TRUE
  } else if (is.na(x)) {
    anyNA(value)
  } else {
    near(sum(value, na.rm = TRUE), x - 1)
  }
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

valid_product_names <- function(x, period = gl(1, length(x))) {
  x <- as.character(x)
  period <- as.factor(period)
  if (anyNA(x) || any(x == "")) {
    stop("each product must have a non-missing name")
  }
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
  if (is.character(i)) {
    res <- match(i, x)
  } else {
    res <- match(x[i], x)
  }
  if (length(res) == 0L) {
    stop("attempted to select less than one element")
  }
  if (anyNA(res)) {
    stop("subscript out of bounds")
  }
  res
}

match_dim <- function(what) {
  what <- as.character(what)
  function(x, dim, several = FALSE) {
    if (!several && length(x) != 1L) {
      stop(gettextf("must supply exactly one %s", what))
    } else if (several && length(x) == 0L) {
      stop(gettextf("must supply at least one %s", what))
    }
    i <- match(x, dim)
    no_match <- is.na(i)
    if (any(no_match)) {
      stop(gettextf("'%s' is not a %s", x[no_match][1L], what))
    }
    i
  }
}

match_levels <- match_dim("index level")

match_time <- match_dim("time period")

#---- Generate index ----
index_skeleton <- function(levels, time) {
  index <- rep.int(NA_real_, length(levels))
  rep.int(list(index), length(time))
}

empty_contrib <- function(x) {
  res <- rep.int(list(numeric(0L)), length(x))
  list(res)
}

contrib_skeleton <- function(levels, time) {
  rep.int(empty_contrib(levels), length(time))
}

has_contrib <- function(x) {
  Position(\(x) any(lengths(x) > 0L), x$contrib, nomatch = 0L) > 0L
}

# Backport Reduce
# TODO: Remove once min R version gets bumped.
if (getRversion() < "4.4.0") {
  Reduce <- function (f, x, init, right = FALSE, accumulate = FALSE, simplify = TRUE) {
    mis <- missing(init)
    len <- length(x)
    if (len == 0L) 
      return(if (mis) NULL else init)
    f <- match.fun(f)
    if (!is.vector(x) || is.object(x)) 
      x <- as.list(x)
    ind <- seq_len(len)
    if (mis) {
      if (right) {
        init <- x[[len]]
        ind <- ind[-len]
      }
      else {
        init <- x[[1L]]
        ind <- ind[-1L]
      }
    }
    if (!accumulate) {
      if (right) {
        for (i in rev(ind)) init <- forceAndCall(2, f, x[[i]], 
                                                 init)
      }
      else {
        for (i in ind) init <- forceAndCall(2, f, init, x[[i]])
      }
      init
    }
    else {
      len <- length(ind) + 1L
      out <- vector("list", len)
      if (mis) {
        if (right) {
          out[[len]] <- init
          for (i in rev(ind)) {
            init <- forceAndCall(2, f, x[[i]], init)
            out[[i]] <- init
          }
        }
        else {
          out[[1L]] <- init
          for (i in ind) {
            init <- forceAndCall(2, f, init, x[[i]])
            out[[i]] <- init
          }
        }
      }
      else {
        if (right) {
          out[[len]] <- init
          for (i in rev(ind)) {
            init <- forceAndCall(2, f, x[[i]], init)
            out[[i]] <- init
          }
        }
        else {
          for (i in ind) {
            out[[i]] <- init
            init <- forceAndCall(2, f, init, x[[i]])
          }
          out[[len]] <- init
        }
      }
      if (all(lengths(out) == 1L) && simplify) 
        out <- unlist(out, recursive = FALSE)
      out
    }
  }
}