\name{extract.abstract_index}
\alias{[.abstract_index}
\alias{[.aggregate_index}
\alias{[<-.abstract_index}
\alias{head.abstract_index}
\alias{tail.abstract_index}

\title{Extract and replace index values}

\description{
Methods to extract and replace index values like a matrix.
}

\usage{
\method{[}{abstract_index}(x, i, j)

\method{[}{aggregate_index}(x, i, j)

\method{[}{abstract_index}(x, i, j) <- value

\method{head}{abstract_index}(x, n = 6, ...)

\method{tail}{abstract_index}(x, n = 6, ...)
}

\arguments{
\item{x}{A price index, as made by, e.g., \code{\link[=elemental_index]{elemental_index()}}.}

\item{i, j}{Indices for the levels and time periods of a price index. See details.}

\item{value}{A numeric vector.}

\item{n}{See \code{\link{head}}/\code{\link{tail}}.}

\item{...}{Further arguments passed to or used by methods.}
}

\details{
The extraction methods treat \code{x} as a matrix of index values with (named) rows for each \code{level} and columns for each \code{period} in \code{x}. Unlike a matrix, dimensions are never dropped as subscripting \code{x} always returns an index object. This means that indexing with a matrix is not possible, and only a submatrix can be extracted. As \code{x} is not an atomic vector, indexing with a single index like \code{x[1]} extracts all time periods for that level. Note that subscripting an aggregated index cannot generally preserve the aggregation structure if any levels are removed or rearranged, and in this case the resulting index is \emph{not} an aggregated index.

The replacement methods similarly treat \code{x} as a matrix, and behave the same as replacing values in a matrix (except that \code{value} is coerced to numeric). Note that replacing the values of an index will remove the corresponding percent-change contributions (if any).
}

\value{
An index object.
}

\examples{
prices <- data.frame(
  rel = 1:8,
  period = rep(1:2, each = 4),
  ea = rep(letters[1:2], 4)
)

# Calculate Jevons elemental indexes

epr <- with(prices, elemental_index(rel, period, ea))

# Extract the indexes like a matrix

epr["a", ]

epr[, 2]

epr[1, ] <- 1 # can be useful for doing specific imputations
epr
}