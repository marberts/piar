#---- Extract contributions ----
contrib <- function(x, ...) {
  UseMethod("contrib")
}

contrib.pindex <- function(x, level = levels(x), ...) {
  if (!x$has_contrib) {
    return(NULL)
  }
  level <- as.character(level)
  con <- lapply(x$contrib, `[[`, match.arg(level))
  products <- unique(nested_names(con))
  out <- structure(vector("list", length(con)), names = names(con))
  # initialize 0 contributions for all products in all time periods
  out[] <- list(structure(numeric(length(products)), names = products))
  # then replace with the actual values, so products that didn't sell
  # have 0 and not NA contributions
  res <- Map(replace, out, lapply(con, names), con)
  do.call(cbind, res)
}
