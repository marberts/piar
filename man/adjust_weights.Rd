\name{adjust_weights}
\alias{adjust_weights}

\title{
Adjust weights
}

\description{
Adjust aggregation weights covering multiple time periods in a price index aggregation structure.
}

\usage{
adjust_weights(x, index, chained = TRUE, na.rm = FALSE, r = 1, maxit = 10, tol = 0.0001)
}

\arguments{
\item{x}{A price index aggregation structure. This can be made with \code{\link[=aggregation_structure]{aggregation_structure()}}.}

\item{index}{An aggregate index, as made by \code{\link[=aggregate.index]{aggregate()}}, that covers the time period (usually one year) of the weights in \code{x}.}

\item{chained, na.rm, r}{See \code{\link[=aggregate.index]{aggregate()}}.}

\item{maxit}{The maximum number of iterations (default is 10).}

\item{tol}{The tolerance used to determine if the adjustment converges (default is 0.0001).}
}

\details{
Adjusting the weight for an elemental aggregate involves deflating it to the first period in \code{index} using the average elemental index over all periods in \code{index}. This ensures that the average of the price-updated weights for \code{index} agrees with the weight in \code{x}.

One wrinkle is dealing with missing elemental aggregates that are imputed with their group mean (i.e., when \code{na.rm = TRUE}). In this case, multiple iterations may be needed to get the average of the price-updated weights close to the weights in \code{x}. (Close is defined as the largest absolute difference between the average price-updated weights and the weights in \code{x} less than \code{tol}.)
}

\value{
\code{adjust_weights()} returns a copy of \code{x} with adjusted weights.
}

\seealso{
\code{\link{aggregation_structure}} to help make a price index aggregation structure.

\code{\link[=aggregate.index]{aggregate}} to aggregate price indexes from \code{\link[=elemental_index]{elemental_index()}}.
}

\examples{
prices <- data.frame(rel = 1:8, period = rep(1:2, each = 4), ea = rep(letters[1:2], 4))

# Aggregation structure

pias <- aggregation_structure(list(c("top", "top", "top"), c("a", "b", "c")), 1:3)

# Calculate elemental indexes

epr <- with(prices, elemental_index(rel, period, ea))

# Aggregate

index <- aggregate(epr, pias, na.rm = TRUE)

# Adjust weights and re-aggregate

pias <- adjust_weights(pias, index, na.rm = TRUE)

aggregate(epr, pias, na.rm = TRUE)
}