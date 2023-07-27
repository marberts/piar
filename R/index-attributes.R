levels.index <- function(x) {
  x$levels
}

`levels<-.index` <- function(x, value) {
  stop("cannot replace levels attribute")
}

time.index <- function(x, ...) {
  x$time
}

start.index <- function(x, ...) {
  x$time[1L]
}

end.index <- function(x, ...) {
  x$time[length(x$time)]
}
