\name{randomForest}
\alias{randomForest}
\title{Grow A Random Forest}
\usage{
  randomForest(formula, data, risk, num_trees,
    num_covariates, ...)
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

  \item{...}{other arguments used by \code{makeTree}.}
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
randomForest(Species ~ ., iris, costGini, 10, 2)
}

