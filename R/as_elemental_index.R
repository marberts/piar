as_elemental_index <- function(x, ...) {
  UseMethod("as_elemental_index")
}

as_elemental_index.default <- function(x, ...) {
  as_elemental_index(as.matrix(x), ...)
}

as_elemental_index.matrix <- function(x, ...) {
  storage.mode(x) <- "numeric"
  if (is.null(rownames(x))) rownames(x) <- seq_len(nrow(x))
  if (is.null(colnames(x))) colnames(x) <- seq_len(ncol(x))
  levels <- as.character(rownames(x)) # as.character is for matrices without rows
  periods <- as.character(colnames(x)) # same for columns
  res <- list(index = NULL, contributions = NULL, levels = NULL, periods = NULL)
  res$index <- res$contributions <- 
    structure(vector("list", ncol(x)), names = periods)
  contrib <- structure(rep(list(numeric(0)), length(levels)), names = levels)
  for (i in seq_along(periods)) {
    res$index[[i]] <- x[, i]
    res$contributions[[i]] <- contrib
  }
  res$levels <- levels
  res$periods <- periods
  structure(res, class = c("elemental", "index"))
}

as_elemental_index.aggregate <- function(x, ...) {
  eas <- x$pias$eas
  index <- lapply(x$index, `[`, eas)
  contributions <- lapply(x$contributions, `[`, eas)
  structure(list(index = index, 
                 contributions = contributions, 
                 levels = eas, 
                 periods = x$periods), 
            class = c("elemental", "index"))
}