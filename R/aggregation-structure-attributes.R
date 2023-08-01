weights.aggregation_structure <- function(object,
                                          ea_only = FALSE,
                                          na.rm = FALSE, ...) {
  if (ea_only) {
    return(object$weights)
  }
  res <- vector("list", object$height)
  res[[1L]] <- object$weights

  for (i in seq_along(res)[-1L]) {
    res[[i]] <- vapply(object$child[[i - 1L]],
                       \(z) sum(res[[i - 1L]][z], na.rm = na.rm),
                       numeric(1L))
  }
  rev(res)
}

`weights<-` <- function(object, value) {
  UseMethod("weights<-")
}

`weights<-.aggregation_structure` <- function(object, value) {
  object$weights[] <- as.numeric(value)
  object
}

levels.aggregation_structure <- function(x) {
  x$levels
}

`levels<-.aggregation_structure` <- function(x, value) {
  stop("cannot replace levels attribute")
}
