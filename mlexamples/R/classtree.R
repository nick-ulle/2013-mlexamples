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
    makeSubtree(data, list(cost = f, complexity = costError),  min_split)
}

# Makes a subtree given the data.
makeSubtree <- function(data, f, min_split, min_bucket = min_split %/% 3, 
                        tree) {
    details <- splitDetails(data[1])

    # Make a Tree if we don't have one yet.
    if (missing(tree)) {
        tree <- Tree()
        split_old <- Split(NA_character_, NA_real_, details$decision, NA_real_,
                       details$n)
        tree$setValue(split_old)
    }


    # Decide if we should split, and do splitting stuff.
    if(sum(details$n) >= min_split) {
        split_pt <- bestSplit(data[-1], data[1], f$cost)
        split_var <- names(split_pt)

        split_data <- factor(data[split_var] < split_pt, c(TRUE, FALSE))
        split_data <- split(data, split_data)
        split_n <- vapply(split_data, nrow, NA_integer_)
        split_details <- sapply(split_data, function(x_) splitDetails(x_[1]))
        
        if (all(split_n >= min_bucket)) {
            l_split <- Split(split_var, split_pt, split_details[[1L, 1L]],
                             NA_real_, split_details[[2L, 1L]])
            r_split <- Split(split_var, split_pt, split_details[[1L, 2L]],
                             NA_real_, split_details[[2L, 2L]])
            # Reaching this point means this node is a branch.
            tree$setLeft(l_split)
            tree$setRight(r_split)
            # tree$setChildren()
            tree$goLeft()
            makeSubtree(split_data[[1L]], f, min_split, min_bucket, tree)
            tree$goUp()$goRight()
            makeSubtree(split_data[[2L]], f, min_split, min_bucket, tree)
            return(tree)

#            complexity <- vapply(split_trees, function(s_) s_@value@complexity,
#                                 numeric(3))
#            complexity <- rowSums(complexity)
#            complexity[[1]] <- 
#                (f$complexity(data[1]) * sum(details$n) - complexity[[2]]) /
#                (complexity[[3]] - 1)
#
#            split_old <- Split(split_var_old, split_pt_old, details$decision,
#                               complexity, details$n)
#            tree <- Tree(split_old, split_trees[[1]], split_trees[[2]])
#            return(tree)
        }
    }
    # Reaching this point means this node is a leaf.

    # Complexity is stored as
    #   (collapse point, cost of subtree, number of leaves in subtree).
    # The latter two are necessary for computing collapse points of ancestors.
#    complexity <- c(NA_real_, f$complexity(data[1]) * sum(details$n), 1)
#
#    split_old <- Split(split_var_old, split_pt_old, details$decision,
#                       complexity, details$n)
#    tree <- Tree(split_old)
    return(tree)
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

    return(split_pt)
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
                         c(sum(y_split[1, ] * y_split[2, ]), x_)
                        }, numeric(2))
    splits[, which.min(splits[1, ])] / c(nrow(y), 1)
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

setMethod('predict', 'Branch',
          function(object, data, a, ...) {
              n <- nrow(data)
              if (n <= 0) return(NA_character_)

              split_old <- object@value

              if (split_old@complexity[[1]] > a) { 
                  split <- object@left@value
                  splits <- factor(data[split@split_var] < split@split_pt,
                                   c(TRUE, FALSE))
                  split <- split(data, splits)
                  split[[1]] <- predict(object@left, split[[1]], a, ...)
                  split[[2]] <- predict(object@right, split[[2]], a, ...)
                  return(unsplit(split, splits))
              }

              rep(split_old@decision, n)
          })

setMethod('predict', 'Leaf', 
          function(object, data, a, ...) {
              n <- nrow(data)
              if (n <= 0) return(NA_character_)
              rep(object@value@decision, n)
          })

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
              split <- if (is.na(x@split_var)) '<root>' else 
                  paste0(x@split_var, ' ', x@split_pt)
              complexity <- if (is.na(x@complexity[[1]])) ' *' else
                  paste0(' ', round(x@complexity[[1]], 4))
              paste0(split, ' ',  x@decision,
                     ' (', paste0(x@n, collapse = ', '), ')', complexity)
          })

