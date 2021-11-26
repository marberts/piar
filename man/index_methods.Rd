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


\title{
Methods for index objects
}

\description{
Methods to put index values/quote contributions in a tabular form, chain an index, merge indexes together, and extract useful information.
}

\usage{
\method{[}{index}(x, i, j)

\method{as.data.frame}{index}(x, row.names = NULL, ..., type = c("index", "contributions"))

\method{as.matrix}{index}(x, type = c("index", "contributions"), ...)

\method{cumprod}{index}(x)

\method{merge}{index}(x, y, ...)

\method{stack}{index}(x, y, ...)

\method{unstack}{index}(x, ...)

\method{levels}{index}(x)

\method{time}{index}(x, ...)

\method{start}{index}(x, ...)

\method{end}{index}(x, ...)
}

\arguments{
\item{x, y}{An object of class \code{index}, as made by, e.g., \code{\link[=elemental_index]{elemental_index()}}.}
\item{i, j}{See \link{Extract}.}
\item{type}{A character vector specifying whether index values (the default) or quote contributions should be coerced into a data.frame/matrix.}
\item{row.names}{Same as \code{\link[=as.data.frame]{as.data.frame()}}.}
\item{...}{Further arguments passed to or used by methods.}
}

\value{
The extraction method treats \code{x} as a matrix of index values with (named) rows for each \code{level} and columns for each \code{period} in \code{x}. Unlike a matrix, dimensions are never dropped; indexing \code{x} always returns a matrix. This means that indexing with a matrix is not possible. As \code{x} is not an atomic vector, indexing with a single index like `\code{x[1]} is taken to be the same as \code{x[1, ]}. The replacement method similarly treats \code{x} as a matrix, and behaves the same as replacing values in a matrix (except that \code{value} is coerced to numeric). Note that replacing the values of an index will remove the corresponding quote contributions (if any).

The \code{as.data.frame()} method turns an index/quote contributions into a data frame with three columns: period, level, and value.

The \code{as.matrix()} method turns an index into a matrix with a row for each level and a column for each period. Quote contributions are padded with NAs to fit into a rectangular array.

The \code{cumprod()} method takes the cumulative product of the values in the \code{index} component of \code{x}, and returns a matrix. The result is the same as \code{t(apply(as.matrix(x), 1, cumprod))}.

The \code{merge()} method combines two index object with common time periods, merging together the index values and quote contributions for each time period in \code{x} and \code{y}. The \code{stack()} methods combines two index objects with common levels, stacking index values and quote contributions (and price-update weights if they're aggregated indexes) for each level in \code{y} after those in \code{x}. Both methods return an object of the same class as \code{x}.

The \code{unstack()} method breaks up \code{x} into a list of indexes, one for each period in \code{x}.

The \code{levels()} method extracts the levels of an index.

The \code{time()} method extracts the time periods of the index. The \code{start()} and \code{end()} methods extract the first and last time period.
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
}
