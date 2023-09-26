\name{piar_index}
\alias{piar_index}
\alias{chainable_piar_index}
\alias{direct_piar_index}
\alias{aggregate_piar_index}
\alias{is_index}
\alias{is_aggregate_index}
\alias{is_chainable_index}
\alias{is_direct_index}

\title{Price index objects}

\description{
Classes to represent price indexes.
\itemize{
\item All indexes inherit from the \code{piar_index} virtual class.
\item Period-over-period indexes that can be chained over time inherit from \code{chainable_piar_index}.
\item Fixed-base indexes inherit from \code{direct_piar_index}. 
\item Aggregate price indexes that are the result of aggregating elemental indexes with an aggregation structure further inherit from \code{aggregate_piar_index}.
}
}

\usage{
is_index(x)

is_chainable_index(x)

is_direct_index(x)

is_aggregate_index(x)
}

\arguments{
\item{x}{An object to test.}
}

\details{
The \code{piar_index} object is a list-S3 class with the following components:
\describe{
\item{index}{A named list with an entry for each \code{period} that gives a named vector of index values for each level in \code{ea}.}
\item{contrib}{A named list with an entry for each \code{period}, which itself contains a named list with an entry for each level in \code{ea} with a named vector that gives the additive contribution for each price relative. If \code{contrib = FALSE}, then each of these vectors is of length 0.}
\item{levels}{The levels for \code{ea}.}
\item{time}{The levels for \code{period}.}
}

The \code{aggregate_piar_index} subclass adds:
\describe{
\item{r}{The value for \code{r}.}
\item{pias}{A list containing the \code{child}, \code{parent}, \code{eas}, and \code{height} components of \code{pias}.}
}
}

\value{
\code{is_index()} returns \code{TRUE} if \code{x} inherits from \code{piar_index}.

\code{is_chainable_index()} returns \code{TRUE} if \code{x} inherits from \code{chainable_piar_index}.

\code{is_direct_index()} returns \code{TRUE} if \code{x} inherits from \code{direct_piar_index}.

\code{is_aggregate_index()} returns \code{TRUE} if \code{x} inherits from \code{aggregate_piar_index}.
}

\examples{
prices <- data.frame(
  rel = 1:8,
  period = rep(1:2, each = 4),
  ea = rep(letters[1:2], 4)
)

pias <- aggregation_structure(
  list(c("top", "top", "top"), c("a", "b", "c")), 1:3
)

# Calculate period-over-period Jevons elemental indexes

epr <- with(prices, elemental_index(rel, period, ea))

# Aggregate as an arithmetic index

index <- aggregate(epr, pias, na.rm = TRUE)

is_index(epr)
is_chainable_index(epr)
is_direct_index(epr)
is_aggregate_index(epr)

is_index(index)
is_chainable_index(index)
is_direct_index(index)
is_aggregate_index(index)
}
