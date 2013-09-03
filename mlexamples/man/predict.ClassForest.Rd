\name{predict.ClassForest}
\alias{predict.ClassForest}
\title{Predict Method For Random Forests}
\usage{
  \method{predict}{ClassForest} (object, data, ...)
}
\arguments{
  \item{object}{a ClassForest object, which will be used to
  make the prediction.}

  \item{data}{a data.frame of new data for which to make
  predictions.}

  \item{...}{reserved for future use.}
}
\value{
  A vector of predictions, one for each row of \code{data}.
}
\description{
  Predict Method For Random Forests
}

