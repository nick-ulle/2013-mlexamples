# cv.R
# Author: Nick Ulle

#' Cross-Validation Framework
#'
#' This function performs k-fold cross-validation, optionally over the range of
#' some tuning parameter.
#'
#' The training function is called once for each fold. The validation function
#' is called once for each tuning parameter value for each fold.
#'
#' @param data a data.frame, containing the data to be used for the 
#' cross-validation.
#' @param train a function, which given a training set data.frame, returns a 
#' fitted model suitable for the validation function.
#' @param validate a function, which given a tuning parameter, a fitted model,
#' and a validation set data.frame, returns a numeric measure of error or risk.
#' This function should accept a tuning parameter even if it is not used.
#' @param tuning an optional vector of tuning parameters to cross-validate on.
#' @param folds the number of folds, k, over which to perform the
#' cross-validation.
#'
#' @export
crossValidate <- function(data, train, validate, tuning = NA, folds = 10L) {
    # First divide the data into folds.
    n <- nrow(data)
    fold_size <- rep.int(n %/% folds, folds)
    remainder <- seq_len(n %% folds)
    fold_size[remainder] <- fold_size[remainder] + 1L
    fold_indices <- split(sample.int(n), rep.int(seq_len(folds), fold_size))

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

    if (is.na(tuning)) rbind('Estimate' = estimate, 'Std. Error' = std_err)
    else rbind('Parameter' = tuning, 'Estimate' = estimate, 
               'Std. Error' = std_err)
}

