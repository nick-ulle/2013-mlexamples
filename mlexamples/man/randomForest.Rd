\name{randomForest}
\alias{randomForest}
\title{Grow A Random Forest}
\usage{
  randomForest(formula, data, risk = riskGini, num_trees,
    num_covariates, min_split = 20L,
    min_bucket = round(min_split/3), prior)
}
\arguments{
  \item{formula}{a formula, with a response and
  covariates.}

  \item{data}{an optional data.frame whose columns are
  variables named in the formula.}

  \item{risk}{a function, to be used for estimating risk
  when growing the tree.}

  \item{num_trees}{the number of trees to grow.}

  \item{num_covariates}{the number of covariates to select
  (randomly) when determining each split.}

  \item{min_split}{the minimum number of observations
  required to make a split.}

  \item{min_bucket}{the minimum number of observations
  required in each leaf.}

  \item{prior}{a numeric vector of prior probabilities for
  each class. This parameter is ignored when doing
  regression.}
}
\value{
  An S3 class ClassForest, representing the forest of
  classification trees.
}
\description{
  This function grows a forest of classification trees from
  data.
}
\examples{
randomForest(Species ~ ., iris, riskGini, 10, 2)
}

