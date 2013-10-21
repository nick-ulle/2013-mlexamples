\name{makeTree}
\alias{makeTree}
\title{Grow A Classification Tree}
\usage{
  makeTree(formula, data, build_risk = riskGini,
    prune_risk = riskError, min_split = 20L,
    min_bucket = round(min_split/3), folds = 10L, prior)
}
\arguments{
  \item{formula}{a formula, with a response and
  covariates.}

  \item{data}{an optional data.frame whose columns are
  variables named in the formula.}

  \item{build_risk}{a function, to be used for estimating
  risk when building the tree.}

  \item{prune_risk}{a function, to be used for estimating
  risk when pruning the tree.}

  \item{min_split}{the minimum number of observations
  required to make a split.}

  \item{min_bucket}{the minimum number of observations
  required in each leaf.}

  \item{folds}{the number of folds to be used in
  cross-validation of the cost-complexity parameter. If
  this is not positive, cross-validation will be skipped.}

  \item{prior}{a numeric vector of prior probabilities for
  each class. This parameter is ignored when doing
  regression.}
}
\value{
  a reference class ClassTree, representing the
  classification tree.
}
\description{
  This function grows a classification tree from data,
  following the CART algorithm described in "Classification
  and Regression Trees". Regression trees and missing data
  are not yet supported.
}
\examples{
# Build a classification tree for Fisher's iris data using defaults.
makeTree(Species ~ ., iris)

# Build a classification tree for the insect spray data using
# misclassification rate instead of Gini impurity, and require at least 25
# observations in every split node.
makeTree(spray ~ count, InsectSprays, build_risk = riskError,
         min_split = 25L)
}

