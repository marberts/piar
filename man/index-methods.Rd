\name{index-methods}
\alias{index-methods}
\alias{[.index}
\alias{[[.index}
\alias{[<-.index}
\alias{[[<-.index}
\alias{levels.index}
\alias{time.index}
\alias{start.index}
\alias{end.index}
\alias{head.index}
\alias{tail.index}
\alias{summary.index}
\alias{as.matrix.index}
\alias{as.data.frame.index}

\title{Methods for price indexes}

\description{
Methods to extract and replace index values, and coerce into a tabular form.
}

\usage{
# Extract
\method{[}{index}(x, i, j)

\method{[[}{index}(x, i, j, exact = TRUE)

\method{[}{index}(x, i, j) <- value

\method{[[}{index}(x, i, j) <- value

\method{levels}{index}(x)

\method{time}{index}(x, ...)

\method{start}{index}(x, ...)

\method{end}{index}(x, ...)

\method{head}{index}(x, n = 6, ...)

\method{tail}{index}(x, n = 6, ...)

# Coerce
\method{as.matrix}{index}(x, ...)

\method{as.data.frame}{index}(x, ..., stringsAsFactors = FALSE)
}

\arguments{
\item{x}{A price index, usually made by \code{\link[=elemental_index]{elemental_index()}}.}

\item{i, j}{Indices for the levels and time periods of a price index. See details.}

\item{exact}{Should levels and time periods be matched exactly when indexing with a character vector? Set to \code{FALSE} to allow partial matching.}

\item{value}{A numeric vector.}

\item{n}{See \code{\link{head}}.}

\item{stringsAsFactors}{See \code{\link{as.data.frame}}.}

\item{...}{Further arguments passed to or used by methods.}
}

\details{
\subsection{Extract}{
The extraction methods treat \code{x} as a matrix of index values with (named) rows for each \code{level} and columns for each \code{period} in \code{x}. Unlike a matrix, dimensions are never dropped as indexing \code{x} always returns an index object (for \code{`[`}) or a single value (for \code{`[[`}). This means that indexing with a matrix is not possible, and only a submatrix can be extracted. As \code{x} is not an atomic vector, indexing with a single index like \code{x[1]} is taken to be the same as \code{x[1, ]}. Note that indexing an aggregated index cannot generally preserve the aggregation structure if any levels are removed or rearranged, and in this case the resulting index is \emph{not} an aggregated index.

The replacement methods similarly treat \code{x} as a matrix, and behaves the same as replacing values in a matrix (except that \code{value} is coerced to numeric). Note that replacing the values of an index will remove the corresponding percent-change contributions (if any).

The \code{levels()} method extracts the levels of an index, and the \code{time()} method extracts the time periods of the index. The \code{start()} and \code{end()} methods extract the first and last time period.

The \code{head()} and \code{tail()} methods act as if \code{x} is a matrix of index values, and by default extract the time series for the first/last six levels of \code{x}.

The \code{summary()} method summarizes \code{x} as a matrix of index values (i.e., the five-number summary for each period). If there are percent-change contributions, then these are also summarized as a matrix.
}

\subsection{Coerce}{
The \code{as.matrix()} method turns an index into a matrix with a row for each level and a column for each period. The \code{as.data.frame()} method turns an index into a data frame with three columns: period, level, and value. 
}
}

\value{
\code{`[`}, \code{`[<-`}, \code{`[[<-`}, \code{head()}, and \code{tail()} all return an index object.

\code{`[[`} returns a single value.

\code{levels()}, \code{time()}, \code{start()}, and \code{end()} return a character vector.

\code{as.data.frame()} returns a data frame.

\code{as.matrix()} returns a matrix.
}

\seealso{
\code{\link{elemental_index}} to make an elemental index.

\code{\link{as_index}} to coerce into an index object.

\code{\link{chain}} for chaining period-over-period indexes, and \code{\link{rebase}} for rebasing an index.

\code{\link{contrib}} for extracting percent-change contributions.

\code{\link{combine-indexes}} to merge and stack index values.
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

epr[["a", 2]]

epr[1, ] <- 1 # can be useful for doing specific imputations

# Extract useful features of the index

head(epr, 1)
tail(epr, 3)
levels(epr)
time(epr)
start(epr)
end(epr)

summary(epr)

# Turn the index into a data frame/matrix

as.data.frame(epr)
as.matrix(epr)
}