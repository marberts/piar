\name{price_relative}
\alias{price_relative}

\title{
Price relative
}

\description{
Construct period-over-period price relatives from information on prices over time.
}

\usage{
price_relative(x, period, product)
}

\arguments{
\item{x}{A numeric vector of prices.}
\item{period}{A factor, or something that can be coerced into one, that gives the corresponding time period for each element in \code{x}. The ordering of time periods follows the levels of \code{period} to agree with \code{\link[=cut.Date]{cut()}}.}
\item{product}{A factor, or something that can be coerced into one, that gives the corresponding product identifier for each element in \code{x}.}
}

\value{
A numeric vector of price relatives, with \code{product} as names.
}

\seealso{
\code{\link[=back_price]{back_price}} to get only the back price.

\code{\link[=base_price]{base_price}} for making fixed-base price relatives.

\code{\link{carry_forward}} and \code{\link{shadow_price}} for imputations for missing prices.
}

\examples{
price_relative(1:6, rep(1:2, each = 3), rep(letters[1:3], 2))
}
