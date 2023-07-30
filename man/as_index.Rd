\name{as_index}
\alias{as_index}
\alias{as_index.default}
\alias{as_index.matrix}
\alias{as_index.data.frame}
\alias{as_index.chainable_index}
\alias{as_index.direct_index}
\alias{is_index}
\alias{is_aggregate_index}
\alias{is_chainable_index}
\alias{is_direct_index}

\title{Coerce to a price index}

\description{
Coerce pre-computed index values into an index object, and test if an object is a price index.
}

\usage{
as_index(x, ...)

\method{as_index}{default}(x, chainable = TRUE, ...)

\method{as_index}{matrix}(x, chainable = TRUE, ...)

\method{as_index}{data.frame}(x, cols = 1:3, chainable = TRUE, ...)

\method{as_index}{chainable_index}(x, chainable = TRUE, ...)

\method{as_index}{direct_index}(x, chainable = FALSE, ...)

is_index(x)

is_chainable_index(x)

is_direct_index(x)

is_aggregate_index(x)
}

\arguments{
\item{x}{An object to test or coerce into a price index.}

\item{chainable}{Are the index values in \code{x} period-over-period indexes, suitable for a chained calculation (the default)? This should be \code{FALSE} when \code{x} is a fixed-base (direct) index.}

\item{cols}{A vector giving the positions/names of the period, level, and value columns in \code{x}. The default assumes that the first column contains time periods, the second contains levels, and the third contains index values.}

\item{...}{Further arguments passed to or used by methods.}
}

\details{
Numeric matrices are coerced into an index object by treating each column as a separate time period, and each row as an elemental aggregate. Column names are used to denote time periods, and row names are used to denote elemental aggregates (so they must be unique). This essentially reverses calling \code{\link[=as.matrix.index]{as.matrix()}} on an index object. If a dimension is unnamed, then it is given a sequential label from 1 to the size of that dimension. The default method coerces \code{x} to a matrix prior to using the matrix method.

The data frame method for \code{as_index()} is best understood as reversing the effect of \code{\link[=as.data.frame.index]{as.data.frame()}} on an index object. It constructs a matrix by taking the levels of \code{x[[cols[1]]]} as columns and the levels of \code{x[[cols[2]]]} as rows (coercing to a factor if necessary). It then populates this matrix with the corresponding values in \code{x[[cols[3]]]}, and uses the matrix method for \code{as_index()}.

If \code{x} is a period-over-period index then it is returned unchanged when \code{chainable == TRUE} and chained otherwise. Similarly, if \code{x} is a fixed-base index then it is returned unchanged when \code{chainable == FALSE} and unchain otherwise.
}

\value{
\code{as_index()} returns an object of class \code{chainable_index} if \code{chainable == TRUE}, or \code{direct_index} if \code{chainable == FALSE}, inheriting from class \code{index}.

\code{is_index()} returns \code{TRUE} if \code{x} inherits from \code{index}.

\code{is_chainable_index()} returns \code{TRUE} if \code{x} inherits from \code{chainable_index}.

\code{is_direct_index()} returns \code{TRUE} if \code{x} inherits from \code{direct_index}.

\code{is_aggregate_index()} returns \code{TRUE} if \code{x} inherits from \code{aggregate_index}.
}

\seealso{
\code{\link[=as.matrix.index]{as.matrix}} and \code{\link[=as.data.frame.index]{as.data.frame}} for coercing an index into a tabular form.
}

\examples{
prices <- data.frame(
  rel = 1:8,
  period = rep(1:2, each = 4),
  ea = rep(letters[1:2], 4)
)

pias <- aggregation_structure(
  list(c("top", "top", "top"), c("a", "b", "c")), 1:3
)

# Calculate period-over-period Jevons elemental indexes

epr <- with(prices, elemental_index(rel, period, ea))

# Aggregate as an arithmetic index

index <- aggregate(epr, pias, na.rm = TRUE)

is_index(epr)
is_chainable_index(epr)
is_aggregate_index(epr)

is_index(index)
is_chainable_index(index)
is_aggregate_index(index)

all.equal(as_index(as.data.frame(epr)), epr)
all.equal(as_index(as.matrix(epr)), epr)
}