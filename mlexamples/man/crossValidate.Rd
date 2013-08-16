\name{crossValidate}
\alias{crossValidate}
\title{Cross-Validation Framework}
\usage{
  crossValidate(data, train, validate, tuning = NA,
    folds = 10L)
}
\arguments{
  \item{data}{a data.frame, containing the data to be used
  for the cross-validation.}

  \item{train}{a function, which given a training set
  data.frame, returns a fitted model suitable for the
  validation function.}

  \item{validate}{a function, which given a tuning
  parameter, a fitted model, and a validation set
  data.frame, returns a numeric measure of error or risk.
  This function should accept a tuning parameter even if it
  is not used.}

  \item{tuning}{an optional vector of tuning parameters to
  cross-validate on.}

  \item{folds}{the number of folds, k, over which to
  perform the cross-validation.}
}
\description{
  This function performs k-fold cross-validation,
  optionally over the range of some tuning parameter.
}
\details{
  The training function is called once for each fold. The
  validation function is called once for each tuning
  parameter value for each fold.
}

