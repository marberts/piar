\name{index-methods}
\alias{index-methods}
\alias{merge.index}
\alias{stack.index}
\alias{unstack.index}
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
Methods to combine price indexes together, extract useful information, replace index values, and coerce into a tabular form.
}

\usage{
# Combine
\method{merge}{index}(x, y, ...)

\method{stack}{index}(x, y, ...)

\method{unstack}{index}(x, ...)

# Extract
\method{[}{index}(x, i, j)

\method{[[}{index}(x, i, j)

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
\item{x, y}{A price index, usually made by \code{\link[=elemental_index]{elemental_index()}}.}

\item{i, j}{Indices for the levels and time periods of a price index. See details.}

\item{value}{A numeric vector.}

\item{n}{See \code{\link{head}}.}

\item{stringsAsFactors}{See \code{\link{as.data.frame}}.}

\item{...}{Further arguments passed to or used by methods.}
}

\details{
\subsection{Combine}{
The \code{merge()} method combines two index objects with common time periods, merging together the index values and quote contributions for each time period in \code{x} and \code{y}. This is useful for building up an index when different elemental aggregates come from different sources of data, or use different index-number formulas. 

The \code{stack()} method combines two index objects with common levels, stacking index values and quote contributions for each level in \code{y} after those in \code{x}. The \code{unstack()} method breaks up \code{x} into a list of indexes, one for each period in \code{x}. These methods can be used in a map-reduce to make an index with multiple aggregation structures (like a Paasche index).

It is not generally possible to merge aggregated indexes, as this would change the aggregation structure, so merging does not return aggregated index. If at least one of \code{x} or \code{y} is an aggregate index then the result of stacking these indexes is also an aggregate index; otherwise, it is the same class as \code{x}.
}

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
\code{merge()}, \code{stack()}, \code{unstack()}, \code{`[`}, \code{`[<-`}, \code{`[[<-`}, \code{head()}, and \code{tail()} all return an index object.

\code{`[[`]} returns a single value.

\code{levels()}, \code{time()}, \code{start()}, and \code{end()} return a character vector.

\code{as.data.frame()} returns a data frame.

\code{as.matrix()} returns a matrix.
}

\seealso{
\code{\link{elemental_index}} to make an elemental index.

\code{\link{as_index}} to coerce into an index object.

\code{\link{chain}} for chaining period-over-period indexes, and \code{\link{rebase}} for rebasing an index.

\code{\link{contrib}} for extracting quote contributions.
}

\examples{
prices <- data.frame(rel = 1:8, period = rep(1:2, each = 4), ea = rep(letters[1:2], 4))

# Calculate Jevons elemental indexes

epr <- with(prices, elemental_index(rel, period, ea))

# Extract the indexes like a matrix

epr["a", ]

epr[, 2]

epr[1, ] <- 1 # can be useful for doing specific imputations

# Merge two indexes prior to aggregation

prices2 <- data.frame(rel = 1:8, period = rep(1:2, each = 4), ea = rep(letters[3:4], 4))
epr2 <- with(prices2, elemental_index(rel, period, ea))
merge(epr, epr2)

# Stack two indexes prior to aggregation

prices3 <- data.frame(rel = 1:8, period = rep(3:4, each = 4), ea = rep(letters[1:2], 4))
epr3 <- with(prices3, elemental_index(rel, period, ea))
stack(epr, epr3)

# Unstack does the reverse

all.equal(c(unstack(epr), unstack(epr3)), unstack(stack(epr, epr3)))

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