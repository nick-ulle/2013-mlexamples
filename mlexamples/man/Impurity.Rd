\name{impurityError}
\alias{impurityEntropy}
\alias{impurityError}
\alias{impurityGini}
\title{Impurity Functions}
\usage{
  impurityError(y)

  impurityGini(y)

  impurityEntropy(y)
}
\arguments{
  \item{y}{a single column data.frame or vector of
  categorical response values.}
}
\value{
  a list containing the node decision, misclassification
  error, impurity, and size.
}
\description{
  These functions compute the impurity of a node.
}

