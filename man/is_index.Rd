% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/index-class.R
\name{is_index}
\alias{is_index}
\alias{is_chainable_index}
\alias{is_direct_index}
\title{Test if an object is a price index}
\usage{
is_index(x)

is_chainable_index(x)

is_direct_index(x)
}
\arguments{
\item{x}{An object to test.}
}
\value{
\code{is_index()} returns \code{TRUE} if \code{x} inherits from \code{\link{piar_index}}.

\code{is_chainable_index()} returns \code{TRUE} if \code{x} inherits from
\code{\link{chainable_piar_index}}.

\code{is_direct_index()} returns \code{TRUE} if \code{x} inherits from
\code{\link{direct_piar_index}}.
}
\description{
Test if an object is a index object or a subclass of an index object.
}
