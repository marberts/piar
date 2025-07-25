% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split_classification.R
\name{split_classification}
\alias{split_classification}
\title{Split a hierarchical classification}
\usage{
split_classification(x, split, ..., sep = ".", pad = NA)
}
\arguments{
\item{x}{A character vector, or something that can be coerced into one, of
codes/labels for a specific level in a classification (e.g., 5-digit
COICOP).}

\item{split}{A regular expression to delineate and split the levels in \code{x}.
See \code{\link[=strsplit]{strsplit()}}.}

\item{...}{Additional argument to pass to \code{\link[=strsplit]{strsplit()}}.}

\item{sep}{A character used to delineate levels in \code{x} in the result. The
default separates levels by '.'.}

\item{pad}{A string used to pad the shorter labels for an unbalanced
classification. The default pads with NA.}
}
\value{
A list with a entry for each level in \code{x} giving the "digits" that
represent each level in the hierarchy.
}
\description{
Expand a character representation of a hierarchical classification to make a
price index aggregation structure by splitting along a delimiter.
}
\examples{
#' # A simple classification structure
#            1
#      |-----+-----|
#      11          12
#  |---+---|       |
#  111     112     121

split_classification(c("111", "112", "121"), "")

# Useful if there are delimiters in the classification (like COICOP)

split_classification(c("01.1.1", "01.1.2", "01.2.1"), ".", fixed = TRUE)

}
\seealso{
\code{\link[=aggregation_structure]{aggregation_structure()}} to make a price-index aggregation structure.

\code{\link[=expand_classification]{expand_classification()}} to expand a classification by the width of the
levels.
}
