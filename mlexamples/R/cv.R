# cv.R
# Author: Nick Ulle

#' Cross-Validation Framework
#'
#' This function performs k-fold cross-validation, optionally over the range of
#' some tuning parameter.
#'
#' @param data
#' @param tuning
#' @param train
#' @param validate
#' @param folds
#' @export
crossValidate <- function(data, tuning, train, validate, folds = 10L) {
    # First divide the data into folds.
    n <- nrow(data)
    fold_size <- rep.int(n %/% folds, folds)
    remainder <- seq_len(n %% folds)
    fold_size[remainder] <- fold_size[remainder] + 1L
    fold_indices <- split(sample.int(n), rep.int(1:folds, fold_size))

    # Now fit on each fold and get risk for each value of tuning parameter.
    risk <- vapply(fold_indices,
           function(i_) {
               training_set <- data[-i_, ]
               test_set <- data[i_, ]
               model <- train(training_set)
               vapply(tuning, validate, NA_real_, model, test_set)
           }, numeric(length(tuning)))

    # Finally, compute estimates and standard errors.
    estimate <- rowMeans(risk)
    std_err <- sqrt(apply(risk, 1L, var) / folds)
    structure(rbind(estimate, std_err), 
              dimnames = list(c('Estimate', 'Std. Error'), tuning)
              )
}

