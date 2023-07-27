\name{update.aggregation_structure}
\alias{update.aggregation_structure}

\title{
Update a price index aggregation structure
}

\description{
Price update the weights in a price index aggregation structure
}

\usage{
\method{update}{aggregation_structure}(
  object,
  index,
  period = end(index),
  r = NULL, 
  ...
)
}

\arguments{
\item{object}{A price index aggregation structure, as made by \code{\link[=aggregation_structure]{aggregation_structure()}}.}

\item{index}{An aggregate price index, as made by \code{\link[=aggregate.index]{aggregate()}}.}

\item{period}{The time period used to price update the weights. The default uses the last period in \code{index}.}

\item{r}{Order of the generalized mean to update the weights.}

\item{...}{Further arguments passed to or used by methods.}
}

\value{
A copy of \code{object} with price-updated weights using the index values in \code{index}.
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

aggregation_weights <- data.frame(
  level1 = c("1",   "1",   "1"),
  level2 = c("11",  "11",  "12"),
  ea     = c("111", "112", "121"),
  weight = c(1,     3,     4)
)

pias <- as_aggregation_structure(aggregation_weights)

index <- as_index(
  matrix(1:9, 3, dimnames = list(c("111", "112", "121"), NULL))
)

weights(pias)

weights(update(pias, index))
}