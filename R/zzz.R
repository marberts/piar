# Allow index objects to display correctly in the RStudio viewer
View <- function(x, title) {
  UseMethod("View")
}

View.default <- function(...) {
  get("View", "package:utils")(...)
}

View.index <- function(x, title) {
  if (missing(title)) {
    title <- deparse(substitute(x))[1]
  }
  View(as.matrix(x), title)
}
