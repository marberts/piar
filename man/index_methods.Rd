\name{index methods}
\alias{index_methods}
\alias{as.data.frame.index}
\alias{as.matrix.index}
\alias{cumprod.index}
\alias{merge.index}
\alias{stack.index}
\alias{unstack.index}

\title{
Methods for index objects
}

\description{
Methods to put index values/quote contributions in a tabular form, chain an index, and merge indexes together.
}

\usage{
\method{as.data.frame}{index}(x, row.names = NULL, ..., type = c("index", "contributions"))

\method{as.matrix}{index}(x, type = c("index", "contributions"), ...)

\method{cumprod}{index}(x)

\method{merge}{index}(x, y, ...)

\method{stack}{index}(x, y, ...)

\method{unstack}{index}(x, ...)
}

\arguments{
\item{x, y}{An object of class \code{index}, as made by, e.g., \code{\link[=elemental_index]{elemental_index()}}.}
\item{type}{A character vector specifying whether index values (the default) or quote contributions should be coerced into a data.frame/matrix.}
\item{row.names}{Same as \code{\link[=as.data.frame]{as.data.frame()}}.}
\item{...}{Further arguments passed to or used by methods.}
}

\value{
The \code{as.data.frame()} method turns an index/quote contributions into a data frame with three columns: period, level, and value.

The \code{as.matrix()} method turns an index into a matrix with a row for each level and a column for each period. Quote contributions are padded with NAs to fit into a rectangular array.

The \code{cumprod()} method takes the cumulative product of the values in the \code{index} component of \code{x}, and returns a matrix. The result is the same as \code{t(apply(as.matrix(x), 1, cumprod))}.

The \code{merge()} method combines two index object with common time periods, merging together the index values and quote contributions (and price-update weights if they're aggregated indexes) for each time period in \code{x} and \code{y}. The \code{stack()} methods combines two index objects with common levels, stacking index values and quote contributions (and price-update weights if they're aggregated indexes) for each level in \code{y} after those in \code{x}. Both methods return an object of the same class as \code{x}.

The \code{unstack()} method breaks up \code{x} into a list of indexes, one for each period in \code{x}.
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
}
