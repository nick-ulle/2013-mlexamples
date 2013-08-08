\name{makeTree}
\alias{makeTree}
\title{Build A Classification Tree}
\usage{
  makeTree(data, f, min_split)
}
\arguments{
  \item{data}{a data.frame whose first column is the
  categorical response and whose other columns are ordinal
  covariates.}

  \item{f}{an impurity function. See \code{impurityError}
  for details.}

  \item{min_split}{minimum number of observations required
  to make a split.}
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
makeTree(iris[c(5, 1:4)], impurityGini, 50)

makeTree(InsectSprays[c(2, 1)], impurityError, 25)
}

