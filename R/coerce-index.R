as.data.frame.abstract_index <- function(x, ..., stringsAsFactors = FALSE) {
  value <- unlist(x$index, use.names = FALSE)
  period <- rep(x$time, each = length(x$levels))
  data.frame(period, level = x$levels, value,
             stringsAsFactors = stringsAsFactors)
}

as.matrix.abstract_index <- function(x, ...) {
  do.call(cbind, x$index)
}

# not documented
as.double.abstract_index <- function(x, ...) {
  as.double(as.matrix(x), ...)
}
