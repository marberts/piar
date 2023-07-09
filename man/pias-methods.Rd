\name{pias-methods}
\alias{pias-methods}
\alias{weights.pias}
\alias{weights<-}
\alias{weights<-.pias}
\alias{update.pias}
\alias{as.matrix.pias}
\alias{as.data.frame.pias}

\title{
Methods for price index aggregation structures
}

\description{
Functions to work with price index aggregation structures.
\itemize{
\item{Extract, replace, or price update the weights.}
\item{Coerce to an aggregation matrix or data frame.}
}
}

\usage{
\method{weights}{pias}(object, ea_only = FALSE, na.rm = FALSE, ...)

\method{weights}{pias}(object) <- value

\method{update}{pias}(object, index, period = end(index), ...)

\method{as.matrix}{pias}(x, ...)

\method{as.data.frame}{pias}(x, ..., stringsAsFactors = FALSE)
}

\arguments{
\item{object, x}{A price index aggregation structure, as made by \code{\link[=aggregation_structure]{aggregation_structure()}}.}

\item{ea_only}{Should weights be returned for only the elemental aggregates? The default gives the weights for the entire aggregation structure.}

\item{na.rm}{Should missing values be removed from when aggregating the weights (i.e., when \code{ea_only = FALSE})? By default, missing values are not removed.}

\item{value}{A numeric vector of weights for the elemental aggregates of \code{object}.}

\item{index}{An aggregate price index, as made by \code{\link[=aggregate.pindex]{aggregate()}}.}

\item{period}{The time period used to price update the weights. The default uses the last period in \code{index}.}

\item{stringsAsFactors}{See \code{\link{as.data.frame}}.}

\item{...}{Further arguments passed to or used by methods.}
}

\value{
\code{weights()} returns a list with a named vector of weights for each level in the aggregation structure. If \code{ea_only = TRUE} then the return value is just a named vector of weights for the elemental aggregates. The replacement method replaces these values without changing the aggregation structure.

\code{update()} returns a copy of \code{object} with price-updated weights using the index values in \code{index}.

\code{as.matrix()} represents an aggregation structure as a matrix, such that multiplying with a (column) vector of elemental indexes gives the aggregated index.

\code{as.data.frame()} takes an aggregation structure and returns a data frame that could have generated it, with columns \code{level1}, \code{level2}, ..., \code{ea}, and \code{weight}.
}

\seealso{
\code{\link{aggregation_structure}} to make a price index aggregation structure.
}

\examples{
# A simple aggregation structure
#            1
#      |-----+-----|
#      11          12
#  |---+---|       |
#  111     112     121
#  (1)     (3)     (4)

aggregation_weights <- data.frame(level1 = c("1",   "1",   "1"),
                                  level2 = c("11",  "11",  "12"),
                                  ea     = c("111", "112", "121"),
                                  weight = c(1,     3,     4))

pias <- as_aggregation_structure(aggregation_weights)

# Extract the weights

weights(pias)

# ... or update them

weights(pias) <- 1:3

# Return to a data frame

as.data.frame(pias)

# Or make into an aggregation matrix

as.matrix(pias)
}