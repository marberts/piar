#---- Extract contributions ----
contrib <- function(x, ...) {
  UseMethod("contrib")
}

contrib.piar_index <- function(x, level = levels(x), ...) {
  level <- as.character(level)
  con <- lapply(x$contrib, `[[`, match(match.arg(level), x$levels))
  products <- unique(unlist(lapply(con, names), use.names = FALSE))
  out <- vector("list", length(con))
  names(out) <- x$time
  # initialize 0 contributions for all products in all time periods, then 
  # replace with the actual values so products that didn't sell have 0 and 
  # not NA contributions
  out[] <- list(structure(numeric(length(products)), names = products))
  res <- Map(replace, out, lapply(con, names), con)
  do.call(cbind, res)
}
