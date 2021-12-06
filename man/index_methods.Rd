\name{index_methods}
\alias{index_methods}
\alias{[.index}
\alias{[<-.index}
\alias{as.data.frame.index}
\alias{as.matrix.index}
\alias{cumprod.index}
\alias{merge.index}
\alias{stack.index}
\alias{unstack.index}
\alias{levels.index}
\alias{time.index}
\alias{start.index}
\alias{end.index}
\alias{head.index}
\alias{tail.index}
\alias{summary.index}
\alias{contrib}
\alias{contrib.index}

\title{
Methods for index objects
}

\description{
Methods to put index values/quote contributions in a tabular form, chain an index, merge indexes together, and extract useful information.
}

\usage{
\method{[}{index}(x, i, j)

\method{[}{index}(x, i, j) <- value

\method{as.data.frame}{index}(x, ...)

\method{as.matrix}{index}(x, ...)

\method{cumprod}{index}(x)

\method{merge}{index}(x, y, ...)

\method{stack}{index}(x, y, ...)

\method{unstack}{index}(x, ...)

\method{levels}{index}(x)

\method{time}{index}(x, ...)

\method{start}{index}(x, ...)

\method{end}{index}(x, ...)

\method{head}{index}(x, ...)

\method{tail}{index}(x, ...)

\method{summary}{index}(object, ...)

contrib(x, ...)

\method{contrib}{index}(x, level = levels(x), ...)
}

\arguments{
\item{x, y, object}{An object of class \code{index}, as made by, e.g., \code{\link[=elemental_index]{elemental_index()}}.}
\item{i, j, value}{See \link{Extract}, with \code{value} being a numeric vector (or something that can coerced into one.)}
\item{level}{The level of an index for which quote contributions are desired.}
\item{...}{Further arguments passed to or used by methods.}
}

\value{
The extraction method treats \code{x} as a matrix of index values with (named) rows for each \code{level} and columns for each \code{period} in \code{x}. Unlike a matrix, dimensions are never dropped; indexing \code{x} always returns a matrix. This means that indexing with a matrix is not possible, and only a submatrix can be extracted. As \code{x} is not an atomic vector, indexing with a single index like `\code{x[1]} is taken to be the same as \code{x[1, ]}. The replacement method similarly treats \code{x} as a matrix, and behaves the same as replacing values in a matrix (except that \code{value} is coerced to numeric). Note that replacing the values of an index will remove the corresponding quote contributions (if any).

The \code{as.data.frame()} method turns an index into a data frame with three columns: period, level, and value. The \code{as.matrix()} method turns an index into a matrix with a row for each level and a column for each period.

The \code{cumprod()} method takes the cumulative product of the values in the \code{index} component of \code{x}, and returns a matrix. The result is the same as \code{t(apply(as.matrix(x), 1, cumprod))}.

The \code{merge()} method combines two index object with common time periods, merging together the index values and quote contributions for each time period in \code{x} and \code{y}. 

The \code{stack()} methods combines two index objects with common levels, stacking index values and quote contributions (and price-update weights if they're aggregated indexes) for each level in \code{y} after those in \code{x}. Both methods return an object of the same class as \code{x}. The \code{unstack()} method breaks up \code{x} into a list of indexes, one for each period in \code{x}.

The \code{levels()} method extracts the levels of an index, and the \code{time()} method extracts the time periods of the index. The \code{start()} and \code{end()} methods extract the first and last time period.

The \code{head()} and \code{tail()} methods act as if \code{x} is a matrix of index values, and by default extract the time series for the first/last six levels of \code{x}.

The \code{summary()} method summarizes \code{x} as a matrix of index values (i.e., the five-number summary for each period). If there are quote contributions, then these are also summarized as a matrix.

The \code{contrib()} extracts the \code{contrib} component of an index object, returning a matrix of quote contributions with a column of each \code{period} and a row for each product in \code{level}. Contributions are padded with NAs to fit into a rectangular array.
}

\seealso{
\code{\link{elemental_index}} for making elemental price indexes.
}

\examples{
prices <- data.frame(rel = 1:8, period = rep(1:2, each = 4), ea = rep(letters[1:2], 4))

# A simple elemental price structure using a jevons index

epr <- with(prices, elemental_index(rel, period, ea))

# Turn the index into a rectangular array 

as.data.frame(epr)
as.matrix(epr) # same as epr[]
cumprod(epr) # chained index

# It is easy to rebase an index because columns are time periods

cumprod(epr) / cumprod(epr)[, 2]

# Merge two identical indexes

prices2 <- data.frame(rel = 1:8, period = rep(1:2, each = 4), ea = rep(letters[3:4], 4))
epr2 <- with(prices2, elemental_index(rel, period, ea))
as.matrix(merge(epr, epr2))

# Stack two identical indexes

prices3 <- data.frame(rel = 1:8, period = rep(3:4, each = 4), ea = rep(letters[1:2], 4))
epr3 <- with(prices3, elemental_index(rel, period, ea))
as.matrix(stack(epr, epr3))

# Unstack does the reverse

all.equal(c(unstack(epr), unstack(epr3)), unstack(stack(epr, epr3)))

# Extract useful features of the index

levels(epr)
time(epr)
start(epr)
end(epr)

# Index values can be replaced like a matrix

epr[1, "2"] <- 1
epr

# Summarize an index

summary(with(prices, elemental_index(rel, period, ea, contrib = TRUE)))
}
