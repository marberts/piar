\name{head.piar_index}
\alias{head.piar_index}
\alias{tail.piar_index}

\title{Return the first/last parts of an index}

\description{
Extract the first/last parts of an index as if it were a matrix.
}

\usage{
\method{head}{piar_index}(x, n = 6, ...)

\method{tail}{piar_index}(x, n = 6, ...)
}

\arguments{
\item{x}{A price index, as made by, e.g., \code{\link[=elemental_index]{elemental_index()}}.}

\item{n}{See \code{\link{head}}/\code{\link{tail}}. The default takes the first/last 6 levels of \code{x}.}

\item{...}{Further arguments passed to or used by methods.}
}

\value{
An index object that inherits form \code{x}.
}

\examples{
prices <- data.frame(
  rel = 1:8,
  period = rep(1:2, each = 4),
  ea = rep(letters[1:2], 4)
)

# Calculate Jevons elemental indexes

epr <- with(prices, elemental_index(rel, period, ea))

# Get the first/last time series

head(epr, 1)

tail(epr, 1)
}