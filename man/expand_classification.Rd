\name{expand_classification}
\alias{expand_classification}

\title{
Expand a hierarchical classification
}

\description{
Expand a character representation of a hierarchical classification to make a price index aggregation structure.
}

\usage{
expand_classification(x, width = 1)
}

\arguments{
\item{x}{A character vector, or something that can be coerced into one, of codes/labels for a specific level in a classification (e.g., 5-digit COICOP, 5-digit NAICS, 4-digit SIC).}

\item{width}{An integer vector that gives the width of each digit in \code{x}. A single value is recycled to span the longest element in \code{class}. This cannot contain NAs. The default assumes each digit has a width of 1, as in the NAICS, NAPCS, and SIC classifications.}
}

\value{
A list with a entry for each level in \code{x} giving the "digits" that represent each level in the hierarchy.
}

\seealso{
\code{\link{aggregation_structure}} to make a price-index aggregation structure.
}

\examples{
# A simple classification structure
#            1
#      |-----+-----|
#      11          12
#  |---+---|       |
#  111     112     121

expand_classification(c("111", "112", "121"))

# Expanding more complex classifications
# ... if last 'digit' is either TA or TS

expand_classification(
  c("111TA", "112TA", "121TS"), width = c(1, 1, 1, 2)
)

# ... if first 'digit' is either 11 or 12

expand_classification(c("111", "112", "121"), width = c(2, 1))

# ...if there are delimiters in the classification (like COICOP)

expand_classification(c("01.1.1", "01.1.2", "01.2.1"), width = 2)
}
