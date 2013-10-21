\name{riskError}
\alias{riskEntropy}
\alias{riskError}
\alias{riskGini}
\alias{riskSAE}
\alias{riskSSE}
\alias{riskTwoing}
\title{Risk Functions}
\usage{
  riskError(y, N, prior, avg)

  riskGini(y, N, prior, avg)

  riskEntropy(y, N, prior, avg)

  riskTwoing(y, N, prior, avg)

  riskSSE(y, N, prior, avg)

  riskSAE(y, N, prior, avg)
}
\arguments{
  \item{y}{a factor (for classification) or a numeric
  vector (for regression).}

  \item{N}{a numeric vector of overall sample counts for
  each class.}

  \item{prior}{a numeric vector of prior probabilities for
  each class.}

  \item{avg}{a logical indicating whether to take the
  weighted average of the risk for each side.}
}
\value{
  a numeric cost.
}
\description{
  These functions compute the risk of a split. For
  classification, possible metrics are error, gini
  coefficient, information entropy, and twoing. The former
  three are also available for computing the risk of a node
  by setting the \code{avg} parameter to FALSE. For
  regression, possible metrics are sum of squared error
  (SSE) and sum of absolute error (SAE); these functions
  ignore all parameters except \code{y}.
}
\details{
  Custom risk functions can also be written by the user,
  provided they have the same behavior and signature
  described in this file.
}

