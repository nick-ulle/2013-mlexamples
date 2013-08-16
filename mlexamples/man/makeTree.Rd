\name{makeTree}
\alias{makeTree}
\title{Build A Classification Tree}
\usage{
  makeTree(formula, data, build_risk = costGini,
    prune_risk = costError, min_split = 20,
    min_bucket = min_split\%/\%3)
}
\arguments{
  \item{formula}{a formula, with a response and
  covariates.}

  \item{data}{an optional data.frame whose columns are
  variables named in the formula.}

  \item{build_risk}{risk a function, to be used for
  estimating risk when building the tree.}

  \item{prune_risk}{risk a function, to be used for
  estimating risk when pruning the tree.}

  \item{min_split}{the minimum number of observations
  required to make a split.}

  \item{min_bucket}{the minimum number of observations
  required in each leaf.}
}
\value{
  a reference class ClassTree, representing the
  classification tree.
}
\description{
  This function builds a classification tree from data,
  following techniques described in Hastie, et al, among
  others. Nominal covariates and pruning via
  cross-validation are not yet supported.
}
\examples{
makeTree(Species ~ ., iris)

makeTree(spray ~ count, InsectSprays, build_risk = costError,
         min_split = 25L)
}

