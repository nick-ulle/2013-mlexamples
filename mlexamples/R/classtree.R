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
    details <- f(data[1])
    tree <- Tree(Split('root', NA, details$n, NA_real_, details$decision,
                       details$error))

    if (details$n >= min_split) {
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
    # TODO: move this up the call stack (it only needs to be done once).
    n <- nrow(y)
    x_mid <- sapply(x, sort)
    x_mid <- (x_mid[-n, , drop = FALSE] + x_mid[-1, , drop = FALSE]) / 2
    x_mid <- data.frame(apply(x_mid, 2, unique))

    # Gets the best split.
    splits <- mapply(bestSplitWithin, x_mid, x, MoreArgs = list(y, f))
    split_pt <- splits[2, which.min(splits[1, ])]

    # Sets up the return value.
    split_var <- names(split_pt)
    y_split <- factor(x[split_var] < split_pt, c(TRUE, FALSE),
                      c('left', 'right'))
    y_split <- split(y, y_split)
    y_split <- sapply(y_split, f)

    left <- Split(split_var, split_pt, y_split[[4, 1]], NA_real_, 
                  y_split[[1, 1]], y_split[[2, 1]])
    right <- Split(split_var, split_pt, y_split[[4, 2]], NA_real_,
                   y_split[[1, 2]], y_split[[2, 2]])
    list(left = left, right = right)
}

# Finds best split within one covariate.
bestSplitWithin <- function(x_mid, x, y, f) {
    splits <- sapply(x_mid,
                     function(x_) {
                         split <- numeric(2)
                         l <- y[x < x_, , drop = FALSE]
                         r <- y[x >= x_, , drop = FALSE]
                         l_impurity <- f(l)$impurity
                         r_impurity <- f(r)$impurity

                         split[[1]] <- weighted.mean(c(l_impurity, r_impurity),
                                                     c(nrow(l), nrow(r))
                                                     )
                         split[[2]] <- x_
                         return(split)
                        })
    splits[, which.min(splits[1, ])]
}

# TODO: separate node 'stats' computation from node impurity computation.
# Prototype for separation of computing node 'stats' and node impurity.
proportions <- function(y) {
    y <- table(y)
    proportions <- drop(as.matrix(prop.table(y)))
    y_max <- which.max(proportions)
    # Handles the case where every proportion is NaN, i.e., y is empty.
    if (length(y_max) == 0) y_max <- 1L
    decision <- names(proportions)[[y_max]]
    list(n = sum(y), decision = decision, proportions = proportions)
}

#' Impurity Functions
#'
#' These functions compute the impurity of a node.
#'
#' @param y a single column data.frame or vector of categorical response values.
#' @return a list containing the node decision, misclassification error,
#' impurity, and size.
#' @rdname Impurity
#' @export
impurityError <- function(y) {
    n <- sum(table(y))
    y_props <- prop.table(table(y))
    y_max <- which.max(y_props)
    if (length(y_max) == 0) {
        # Every proportion was NaN, i.e., y is empty.
        decision <- names(y_props)[[1L]]
        error <- 0
        impurity <- 0
    } else {
        decision <- names(y_props)[[y_max]]
        error <- 1 - y_props[[y_max]]
        impurity <- error
    }
    list(decision = decision, error = error, impurity = impurity, n = n)
}

#' @rdname Impurity
#' @export
impurityGini <- function(y) {
    n <- sum(table(y))
    y_props <- prop.table(table(y))
    y_max <- which.max(y_props)
    if (length(y_max) == 0) {
        # Every proportion was NaN, i.e., y is empty.
        decision <- names(y_props)[[1L]]
        error <- 0
        impurity <- 0
    } else {
        decision <- names(y_props)[[y_max]]
        error <- 1 - y_props[[y_max]]
        impurity <- sum(y_props * (1 - y_props))
    }
    list(decision = decision, error = error, impurity = impurity, n = n)
}

#' @rdname Impurity
#' @export
impurityEntropy <- function(y) {
    n <- sum(table(y))
    y_props <- prop.table(table(y))
    y_max <- which.max(y_props)
    if (length(y_max) == 0) {
        # Every proportion was NaN, i.e., y is empty.
        decision <- names(y_props)[[1L]]
        error <- 0
        impurity <- 0
    } else {
        decision <- names(y_props)[[y_max]]
        error <- 1 - y_props[[y_max]]
        impurity <- -sum(y_props * log(y_props))
    }
    list(decision = decision, error = error, impurity = impurity, n = n)
}

setClass('Split',
         representation(split_var = 'character',
                        split_pt = 'ANY',
                        n = 'numeric',
                        complexity = 'numeric',
                        decision = 'character',
                        error = 'numeric'
                        )
         )

Split <- function(split_var, split_pt, n, complexity, decision, error) {
    new('Split', split_var = split_var, split_pt = split_pt, n = n,
        complexity = complexity, decision = decision, error = error)
}

setMethod('as.character', 'Split',
          function(x, ...) {
              paste(x@split_var, x@split_pt, x@n, x@decision, 
                    round(x@error, 4))
          })

