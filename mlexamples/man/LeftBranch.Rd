\name{\%<=\%}
\alias{\%<=\%}
\alias{\%<=\%.default}
\alias{\%<=\%.factor}
\alias{\%<=\%.list}
\title{Left Branch Operator}
\usage{
  %<=%(x, y)

  %<=%.default(x, y)

  %<=%.factor(x, y)

  %<=%.list(x, y)
}
\arguments{
  \item{x}{a vector, the values to be tested.}

  \item{y}{a numeric value or a factor, which defines the
  split.}
}
\value{
  A logical vector of the same length as \code{x}.
}
\description{
  This is a binary operator which returns a logical vector
  indicating whether \code{x} gets sent to the left branch
  under the split defined by \code{y}. In particular, if
  \code{y} is numeric, this tests whether the elements of
  \code{x} are less than or equal to \code{y}, and if
  \code{y} is a factor, this tests whether the elements of
  \code{x} are in \code{y}.
}

