\name{as_elemental_index}
\alias{as_elemental_index}
\alias{as_elemental_index.default}
\alias{as_elemental_index.matrix}

\title{
Coerce to an elemental index
}

\description{
Coerce pre-computed index values to an elemental index object.
}

\usage{
as_elemental_index(x, ...)

\method{as_elemental_index}{default}(x, ...)

\method{as_elemental_index}{matrix}(x, ...)
}

\arguments{
\item{x}{An object to coerce into an elemental index.}

\item{...}{Further arguments passed to or used by methods.}
}

\details{
Numeric matrices are coerce into an elemental index object by treating each column as a separate time period, and each row as an elemental aggregate. Column names are used to denote time periods, and row names are used to denote elemental aggregates. If a dimension is unnamed, then it is given a sequential label from 1 to the size of that dimension.

The default method coerces \code{x} to a matrix prior to using the matrix method.
}

\value{
An object of class \code{elemental}, inheriting from \code{index}.
}

\seealso{
\code{\link{elemental_index}} for making elemental price indexes.
}

\examples{
prices <- data.frame(rel = 1:8, period = rep(1:2, each = 4), ea = rep(letters[1:2], 4))

# A simple elemental price structure using a Jevons index

epr <- with(prices, elemental_index(rel, period, ea))

# Coerece to a matrix

as.matrix(epr)

# Reverse this operation

epr2 <- as_elemental_index(as.matrix(epr))

all.equal(epr, epr2)
}
