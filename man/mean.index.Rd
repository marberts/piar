\name{mean.index}
\alias{mean.index}

\title{Aggregate a price index over subperiods}

\description{
Aggregate an index over subperiods by taking the (usually arithmetic) mean of index values over consecutive windows of subperiods. 
}

\usage{
\method{mean}{index}(x, w = NULL, window = 3, na.rm = FALSE, r = 1, ...)
}

\arguments{
\item{x}{A price index, as made by, e.g., \code{\link[=elemental_index]{elemental_index()}}.}

\item{w}{A numeric vector of weights for the index values in \code{x}. The default is equal weights. It is usually easiest to specify these weights as a matrix with a row for each index value in \code{x} and a column for each time period.}

\item{window}{The size of the window used to average index values across subperiods. The default (3) turns a monthly index into into a quarterly one.}

\item{na.rm}{Should missing values be removed? By default, missing values are not removed. Setting \code{na.rm = TRUE} is equivalent to overall mean imputation.}

\item{r}{Order of the generalized mean to aggregate index values. 0 for a geometric index (the default for making elemental indexes), 1 for an arithmetic index (the default for aggregating elemental indexes and averaging indexes over subperiods), or -1 for a harmonic index (usually for a Paasche index). Other values are possible; see \code{\link[=generalized_mean]{generalized_mean()}} for details.}

\item{...}{Further arguments passed to or used by methods.}
}

\details{
The \code{mean()} method constructs a set of windows of length \code{window}, starting in the first period of the index, and takes the mean of each index value in these windows for each level of the index. The last window is discarded if it is incomplete, so that index values are always averaged over \code{window} periods. The names for the first time period in each window form the new names for the aggregated time periods. Note that percent-change contributions are discarded when aggregating over subperiods. 

An optional vector of weights can be specified when aggregating index values over subperiods, which is often useful when aggregating a Paasche index; see section 4.3 of Balk (2008) for details.
}

\value{
An index of the same type as \code{x}.
}

\references{
Balk, B. M. (2008). \emph{Price and Quantity Index Numbers}. Cambridge University Press.
}

\examples{
prices <- data.frame(
  rel = 1:8,
  period = rep(1:2, each = 4),
  ea = rep(letters[1:2], 4)
)

epr <- with(prices, elemental_index(rel, period, ea))

mean(epr, window = 2)
}