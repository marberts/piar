\name{index-attributes}
\alias{levels.piar_index}
\alias{levels<-.piar_index}
\alias{levels<-.aggregate_piar_index}
\alias{time.piar_index}
\alias{time<-}
\alias{time<-.piar_index}
\alias{start.piar_index}
\alias{end.piar_index}

\title{Get the attributes for a price index}

\description{
Methods to get and set the levels and time periods for a price index.
}

\usage{
\method{levels}{piar_index}(x)

\method{levels}{piar_index}(x) <- value

\method{levels}{aggregate_piar_index}(x) <- value

\method{time}{piar_index}(x, ...)

\method{time}{piar_index}(x) <- value

\method{start}{piar_index}(x, ...)

\method{end}{piar_index}(x, ...)
}

\arguments{
\item{x}{A price index, as made by, e.g., \code{\link[=elemental_index]{elemental_index()}}.}

\item{value}{A character vector, or something that can be coerced into one, giving the replacement levels/time periods for \code{x}.}

\item{...}{Further arguments passed to or used by methods.}
}

\value{
\code{levels()} and \code{time()} return a character vector with the levels and time periods for a price index. \code{start()} and \code{end()} return the first and last time period.

The replacement methods return a copy of \code{x} with the levels/time periods in \code{value}. It's not generally possible to change the levels of an aggregate price index, and in this case replacing the levels does not return an aggregate index.
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