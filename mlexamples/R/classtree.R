# classtree.R
# Author: Nick Ulle

#' @include tree.R
NULL

#' Build A Classification Tree
#'
#' This function builds a classification tree from data, following techniques
#' described in Hastie, et al, among others. Nominal covariates and pruning via
#' cross-validation are not yet supported.
#'
#' @param data a data.frame whose first column is the categorical response
#' and whose other columns are ordinal covariates.
#' @param f an impurity function. See \code{impurityError} for details.
#' @param min_split minimum number of observations required to make a split.
#' @return an S4 object of class Tree, representing the classification tree.
#' @examples
#' makeTree(iris[c(5, 1:4)], impurityGini, 50)
#'
#' makeTree(InsectSprays[c(2, 1)], impurityError, 25)
#' @export
makeTree <- function(data, f, min_split) {
    details <- splitDetails(data[1])
    tree <- Tree(Split('root', NA, details$decision, NA_real_, details$n))

    if (sum(details$n) >= min_split) {
        children <- makeSplit(data, f, min_split)
        tree <- setLeft(tree, children$left)
        tree <- setRight(tree, children$right)
    }
    return(tree)
}

makeSplit <- function(data, f, min_split) {
    # TODO: clean up this function.
    best <- bestSplit(data[-1], data[1], f)

    # Sets up the left subtree.
    left <- Tree(best$left)
    data_left <- data[data[best$left@split_var] <= best$left@split_pt, ,
                      drop = FALSE]
    n_left <- nrow(data_left)

    if (n_left >= min_split) {
        split <- makeSplit(data_left, f, min_split)
        left <- setLeft(left, split$left)
        left <- setRight(left, split$right)
    }

    # Sets up the right subtree.
    right <- Tree(best$right)
    data_right <- data[data[best$right@split_var] > best$right@split_pt, ,
                       drop = FALSE]
    n_right <- nrow(data_right)

    if (n_right >= min_split) {
        split <- makeSplit(data_right, f, min_split)
        right <- setLeft(right, split$left)
        right <- setRight(right, split$right)
    }
    list(left = left, right = right)
}

# Finds best split among all covariates.
bestSplit <- function(x, y, f) {
    # Gets pairwise midpoints of sorted, unique covariate values.
    # TODO: add handling for nominal covariate values.
    # TODO: move sorting up the call stack (it only needs to be done once).
    n <- nrow(y)
    x_mid <- sapply(x, sort)
    x_mid <- (x_mid[-n, , drop = FALSE] + x_mid[-1, , drop = FALSE]) / 2
    x_mid <- lapply(data.frame(x_mid), unique)

    # Gets the best split.
    splits <- mapply(bestSplitWithin, x_mid, x, MoreArgs = list(y, f))
    split_pt <- splits[2, which.min(splits[1, ])]

    # Sets up the return value.
    split_var <- names(split_pt)
    y_split <- factor(x[split_var] < split_pt, c(TRUE, FALSE))
    y_split <- split(y, y_split)
    y_split <- vapply(y_split, splitDetails, list(NA_character_, NA_real_))

    left <- Split(split_var, split_pt, y_split[[1, 1]], NA_real_, 
                  y_split[[2, 1]])
    right <- Split(split_var, split_pt, y_split[[1, 2]], NA_real_,
                   y_split[[2, 2]])
    list(left = left, right = right)
}

# Finds best split within one covariate.
bestSplitWithin <- function(x_mid, x, y, f) {
    splits <- vapply(x_mid,
                     function(x_) { 
                         y_split <- factor(x < x_, c(TRUE, FALSE))
                         y_split <- split(y, y_split)
                         y_split <- vapply(y_split,
                                           function(y_) c(f(y_), nrow(y_)),
                                           numeric(2))
                         c(weighted.mean(y_split[1, ], y_split[2, ]), x_)
                        }, numeric(2))
    splits[, which.min(splits[1, ])]
}

#' Retrieve Split Details
#'
#' \code{splitDetails} retrieves the number of elements and the decision
#' for a split.
splitDetails <- function(y) {
    n <- c(table(y))
    decision <- names(which.max(n))
    list(decision = decision, n = n)
}

#' Cost Functions
#'
#' These functions compute the cost of representing a set with its majority
#' element.
#'
#' @param y a factor.
#' @return a numeric cost.
#' @rdname Cost
#' @export
costError <- function(y) {
    y_prop <- prop.table(table(y))
    cost <- if (any(is.nan(y_prop))) 0 else 1 - max(y_prop)
    return(cost)
}

#' @rdname Cost
#' @export
costGini <- function(y) {
    y_prop <- prop.table(table(y))
    cost <- if (any(is.nan(y_prop))) 0 else sum(y_prop * (1 - y_prop))
    return(cost)
}

#' @rdname Cost
#' @export
costEntropy <- function(y) {
    y_prop <- prop.table(table(y))
    cost <- -y_prop * log(y_prop)
    # Handles the case where y_prop contains zero or NaN.
    cost <- sum(ifelse(is.nan(cost), 0, cost))
    return(cost)
}

setClass('Split',
         representation(split_var = 'character',
                        split_pt = 'ANY',
                        decision = 'character',
                        complexity = 'numeric',
                        n = 'integer'
                        )
         )

Split <- function(split_var, split_pt, decision, complexity, n) {
    new('Split', split_var = split_var, split_pt = split_pt,
        decision = decision, complexity = complexity, n = n)
}

setMethod('as.character', 'Split',
          function(x, ...) {
              paste(x@split_var, x@split_pt, sum(x@n), x@decision, 
                    round(x@complexity, 4))
          })

