\name{aggregation_structure-attributes}
\alias{weights.piar_aggregation_structure}
\alias{weights<-}
\alias{weights<-.piar_aggregation_structure}
\alias{levels.piar_aggregation_structure}

\title{
Methods to get the weights and levels for a price index aggregation structure
}

\description{
Get and set the weights for a price index aggregation structure, or get the levels.
}

\usage{
\method{weights}{piar_aggregation_structure}(object, ea_only = FALSE, na.rm = FALSE, ...)

\method{weights}{piar_aggregation_structure}(object) <- value

\method{levels}{piar_aggregation_structure}(x)
}

\arguments{
\item{object, x}{A price index aggregation structure, as made by \code{\link[=aggregation_structure]{aggregation_structure()}}.}

\item{ea_only}{Should weights be returned for only the elemental aggregates? The default gives the weights for the entire aggregation structure.}

\item{na.rm}{Should missing values be removed from when aggregating the weights (i.e., when \code{ea_only = FALSE})? By default, missing values are not removed.}

\item{value}{A numeric vector of weights for the elemental aggregates of \code{object}.}

\item{...}{Further arguments passed to or used by methods.}
}

\value{
\code{weights()} returns a list with a named vector of weights for each level in the aggregation structure. If \code{ea_only = TRUE} then the return value is just a named vector of weights for the elemental aggregates. The replacement method replaces these values without changing the aggregation structure.

\code{levels()} returns a character vector with the levels for the aggregation structure.
}

\examples{
# A simple aggregation structure
#            1
#      |-----+-----|
#      11          12
#  |---+---|       |
#  111     112     121
#  (1)     (3)     (4)

aggregation_weights <- data.frame(
  level1 = c("1",   "1",   "1"),
  level2 = c("11",  "11",  "12"),
  ea     = c("111", "112", "121"),
  weight = c(1,     3,     4)
)

pias <- as_aggregation_structure(aggregation_weights)

# Extract the weights

weights(pias)

# ... or update them

weights(pias) <- 1:3
weights(pias)
}