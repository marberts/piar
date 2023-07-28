\name{index-attributes}
\alias{levels.index}
\alias{time.index}
\alias{start.index}
\alias{end.index}

\title{Get the attributes for a price index}

\description{
Methods to get the levels and time periods for a price index.
}

\usage{
\method{levels}{index}(x)

\method{time}{index}(x, ...)

\method{start}{index}(x, ...)

\method{end}{index}(x, ...)
}

\arguments{
\item{x}{A price index, usually made by \code{\link[=elemental_index]{elemental_index()}}.}

\item{...}{Further arguments passed to or used by methods.}
}

\value{
\code{levels()} and \code{time()} return a character vector with the levels and time periods for a price index. \code{start()} and \code{end()} return the first and last time period.
}

\examples{
prices <- data.frame(
  rel = 1:8,
  period = rep(1:2, each = 4),
  ea = rep(letters[1:2], 4)
)

epr <- with(prices, elemental_index(rel, period, ea))

levels(epr)
time(epr)
start(epr)
end(epr)
}