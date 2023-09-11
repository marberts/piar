levels.pindex <- function(x) {
  x$levels
}

`levels<-.pindex` <- function(x, value) {
  stop("cannot replace levels attribute")
}

time.pindex <- function(x, ...) {
  x$time
}

start.pindex <- function(x, ...) {
  x$time[1L]
}

end.pindex <- function(x, ...) {
  x$time[length(x$time)]
}
