#---- Extract contributions ----
contrib <- function(x, ...) {
  UseMethod("contrib")
}

contrib.piar_index <- function(x, level = levels(x), ...) {
  if (!has_contrib(x)) {
    return(NULL)
  }
  level <- as.character(level)
  con <- lapply(x$contrib, `[[`, match(match.arg(level), x$levels))
  products <- unique(nested_names(con))
  out <- vector("list", length(con))
  names(out) <- x$time
  # initialize 0 contributions for all products in all time periods
  out[] <- list(structure(numeric(length(products)), names = products))
  # then replace with the actual values, so products that didn't sell
  # have 0 and not NA contributions
  res <- Map(replace, out, lapply(con, names), con)
  do.call(cbind, res)
}
