\name{expand classification}
\alias{expand_classification}

\title{
Expand classification
}

\description{
Turn a digit-wise representation of a hierarchical classification into a list that can be passed to \code{\link[=aggregation_structure]{aggregation_structure()}}.
}

\usage{
expand_classification(x, width = rep(1, max(nchar(x), 0, na.rm = TRUE)))
}

\arguments{
\item{x}{A character vector, or something that can be coerced into one, of codes/labels for a specific level in a classification (e.g., 5-digit NAICS).}

\item{width}{A numeric vector that gives the width of each 'digit' in \code{x}. This cannot contain NAs. The default assumes each digit has a width of 1, as in the NAICS and NAPCS classifications.}
}

\value{
A list with a entry for each level in \code{x} giving the "digits" that represent each level in the hierarchy.
}

\examples{
expand_classification(c("111", "112", "121"))

# The last 'digit' is either TA or TS
expand_classification(c("111TA", "112TA", "121TS"), width = c(1, 1, 1, 2))

# The first 'digit' is either 11 or 12
expand_classification(c("111", "112", "121"), width = c(2, 1))

# Shorter codes are padded with NAs
expand_classification(c("111TA", "112TA", "121"), width = c(2, 1, 2))
}