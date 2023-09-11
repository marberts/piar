# Allow index objects to display correctly in the RStudio viewer
View <- function(x, title) {
  UseMethod("View")
}

View.default <- function(...) {
  get("View", "package:utils")(...)
}

View.pindex <- function(x, title) {
  if (missing(title)) {
    title <- deparse(substitute(x))[1]
  }
  View(unclass(x), title)
}
