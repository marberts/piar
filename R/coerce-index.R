as.data.frame.pindex <- function(x, ..., stringsAsFactors = FALSE) {
  value <- unlist(x$index, use.names = FALSE)
  period <- rep(x$time, each = length(x$levels))
  data.frame(period, level = x$levels, value,
             stringsAsFactors = stringsAsFactors)
}

as.matrix.pindex <- function(x, ...) {
  do.call(cbind, x$index)
}

# not documented
as.double.pindex <- function(x, ...) {
  as.double(as.matrix(x), ...)
}
