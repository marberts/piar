\name{vcov}
\alias{vcov.aggregate_index}

\title{Bootstrap variance for a price index with replicate weights}

\description{
Calculate the sampling variance for an aggregate price index when using a sample of elemental aggregates.
}

\usage{
\method{vcov}{aggregate_index}(object, repweights, mse = TRUE, ...)
}

\arguments{
\item{object}{An aggregate price index, as made by \code{\link[=aggregate.index]{aggregate()}}.}

\item{repweights}{A matrix, or something that can be coerced into one, of bootstrap replicate weights with a row for each elemental aggregate and a column for each replicate.}

\item{mse}{Should covariances be centered off the value of the index in \code{object} (the default), or the mean of the replicates?}

\item{...}{Further arguments passed to or used by methods.}
}

\details{
This function is a simple wrapper to calculate the variance matrix for an aggregated index when bootstrap replicate weights are available for the elemental aggregates. This approach is usually applicable when elemental aggregates are sampled, and provides an estimator of the sampling variance of the price index. It ignores any sampling variance from the elemental indexes (which often use judgmental sampling), and ultimately depends on the method of generating replicate weights. It returns a matrix of variances with a row for each upper-level index and a column for each time period. (Chapters 3 and 4 of Selvanathan and Rao (1994), especially section 4.7, provide analytic variance estimators for some common price indexes that are applicable with simple random sampling.) Note that any missing elemental indexes need to be explicitly imputed prior to using this method, otherwise they will propagate throughout the variance calculation.
}

\value{
A matrix of variances.
}

\references{
Selvanathan, E. A., and Rao, D. S. P. (1994). \emph{Index Numbers: A Stochastic Approach}. MacMillan.
}

\source{
The \code{vcov()} method was influenced by a SAS routine by Justin Francis that was first ported to R by Ambuj Dewan, and subsequently rewritten by Steve Martin.
}

\seealso{
The \code{sps_repweights()} function in the \pkg{sps} package to generate replicates weights when elemental aggregates are sampled using sequential Poisson sampling.
}

\examples{
prices <- data.frame(rel = 1:8, period = rep(1:2, each = 4), ea = rep(letters[1:2], 4))

# A two-level aggregation structure

pias <- aggregation_structure(list(c("top", "top", "top"), c("a", "b", "c")), 1:3)

# Calculate Jevons elemental indexes

(epr <- with(prices, elemental_index(rel, period, ea)))

# Same as using lm() or tapply()

exp(coef(lm(log(rel) ~ ea:factor(period) - 1, prices)))

with(prices, t(tapply(rel, list(period, ea), gpindex::geometric_mean, na.rm = TRUE)))

# Extract the indexes like a matrix

epr["a", ]

epr[, 2]

epr[1, ] <- 1 # can be useful for doing specific imputations

# Aggregate (note the imputation for elemental index 'c')

(index <- aggregate(epr, pias, na.rm = TRUE))

# Aggregation can equivalently be done as matrix multiplication

as.matrix(pias) \%*\% as.matrix(chain(index[letters[1:3]]))

# Merge two indexes prior to aggregation

prices2 <- data.frame(rel = 1:8, period = rep(1:2, each = 4), ea = rep(letters[3:4], 4))
epr2 <- with(prices2, elemental_index(rel, period, ea))
aggregate(merge(epr, epr2), pias)

# Stack two indexes prior to aggregation

prices3 <- data.frame(rel = 1:8, period = rep(3:4, each = 4), ea = rep(letters[1:2], 4))
epr3 <- with(prices3, elemental_index(rel, period, ea))
aggregate(stack(epr, epr3), pias)

# Unstack does the reverse

all.equal(c(unstack(epr), unstack(epr3)), unstack(stack(epr, epr3)))

# Extract useful features of the index

head(index, 1)
tail(index, 3)
levels(index)
time(index)
start(index)
end(index)

summary(index)

# Turn the index into a data frame/matrix

as.data.frame(index)
as.matrix(index)

all.equal(as_index(as.data.frame(epr)), epr)
all.equal(as_index(as.matrix(epr)), epr)

# Calculate a CSWD index (same as the Jevons in this example) 
# as an arithmetic index by constructing appropriate weights

library(gpindex)

# A general function to calculate weights to turn the geometric
# mean of the arithmetic and harmonic mean (i.e., Fisher mean)
# into an arithmetic mean

fw <- grouped(nested_transmute(0, c(1, -1), 1))

with(
    prices, 
    elemental_index(rel, period, ea, fw(rel, group = interaction(period, ea)), r = 1)
)
}