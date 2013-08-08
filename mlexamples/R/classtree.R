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
#' @return a reference class ClassTree, representing the classification tree.
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
    if (missing(tree)) {
        tree <- ClassTree(details$decision, details$n)
    } else {
        tree$decision <- details$decision
        tree$n <- details$n
    }

    # Decide if we should split, and do splitting stuff.
    # In any iteration we need to check that the parent is big enough to split,
    # and that the children are big enough to keep the split.
    if(sum(tree$n) >= min_split) {
        split_pt <- bestSplit(data[-1], data[1], f$cost)
        split_var <- names(split_pt)

        split_data <- factor(data[split_var] < split_pt, c(TRUE, FALSE))
        split_data <- split(data, split_data)
        split_n <- vapply(split_data, nrow, NA_integer_)
        
        if (all(split_n >= min_bucket)) {
            # Reaching this point means this node is a branch.
            tree$addSplit(split_var, split_pt)

            tree$goLeft()
            makeSubtree(split_data[[1L]], f, min_split, min_bucket, tree)
            tree$goUp()

            tree$goRight()
            makeSubtree(split_data[[2L]], f, min_split, min_bucket, tree)
            tree$goUp()
        }
    }

    tree$risk <- f$complexity(data[1L]) * sum(tree$n)
    tree$updateCollapse()
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

ClassTree = setRefClass('ClassTree', contains = c('Tree'),
    fields = list(
        variable = 'character',
        point = 'numeric',
        decision_ = 'character',
        decision = function(x) {
            if (missing(x)) decision_[[cursor]]
            else decision_[[cursor]] <<- x
        },
        risk_ = 'numeric',
        risk = function(x) {
            if (missing(x)) risk_[[cursor]]
            else risk_[[cursor]] <<- x
        },
        leaf_risk_ = 'numeric',
        leaf_risk = function(x) {
            if (missing(x)) leaf_risk_[[cursor]]
            else leaf_risk_[[cursor]] <<- x
        },
        leaf_count_ = 'integer',
        leaf_count = function(x) {
            if (missing(x)) leaf_count_[[cursor]]
            else leaf_count_[[cursor]] <<- x
        },
        collapse_ = 'numeric',
        collapse = function(x) {
            if (missing(x)) collapse_[[cursor]]
            else collapse_[[cursor]] <<- x
        },
        n_ = 'matrix',
        n = function(x) {
            if (missing(x)) n_[cursor, ]
            else n_[cursor, ] <<- x
        }
    ),
    methods = list(
        initialize = function(decision, n, ...) {
            callSuper(n_ = matrix(NA_integer_, 0L, length(n)), ...)
            # Set values.
            variable[[1L]] <<- NA_character_
            point[[1L]] <<- NA_real_
            decision <<- decision
            n <<- n
        },
        increaseReserve = function() {
            callSuper()
            new_length <- length(variable) + mem_reserve
            length(variable) <<- new_length
            length(point) <<- new_length
            length(decision_) <<- new_length
            length(risk_) <<- new_length
            length(leaf_risk_) <<- new_length
            length(leaf_count_) <<- new_length
            length(collapse_) <<- new_length
            n_ <<- rbind(n_, matrix(NA_integer_, mem_reserve, dim(n_)[[2L]]))
        },
        addSplit = function(variable, point) {
            addLeft()
            addRight()
            ids <- c(frame[[cursor, 1L]], frame[[cursor, 2L]])
            variable[ids] <<- variable
            point[ids] <<- point
        },
        updateCollapse = function() {
            ids <- c(frame[[cursor, 1L]], frame[[cursor, 2L]])

            if (any(is.na(ids))) {
                leaf_risk <<- risk
                leaf_count <<- 1L
                collapse <<- Inf
            } else {
                leaf_risk <<- sum(leaf_risk_[ids])
                leaf_count <<- sum(leaf_count_[ids])
                collapse <<- (risk - leaf_risk) / (leaf_count - 1L)
            }
        },
        showSubtree = function(id, level = 0L) {
            if (!is.na(id)) {
                str <- paste0(variable[[id]], ' ', point[[id]], ' ',
                              decision_[[id]])
                cat(rep.int('  ', level), id, ') ', str, '\n', sep = '')
                level <- level + 1L
                showSubtree(frame[[id, 1L]], level)
                showSubtree(frame[[id, 2L]], level)
            }
        },
        predict = function() {
        }
    )
)

#setMethod('predict', 'Branch',
#          function(object, data, a, ...) {
#              n <- nrow(data)
#              if (n <= 0) return(NA_character_)
#
#              split_old <- object@value
#
#              if (split_old@complexity[[1]] > a) { 
#                  split <- object@left@value
#                  splits <- factor(data[split@split_var] < split@split_pt,
#                                   c(TRUE, FALSE))
#                  split <- split(data, splits)
#                  split[[1]] <- predict(object@left, split[[1]], a, ...)
#                  split[[2]] <- predict(object@right, split[[2]], a, ...)
#                  return(unsplit(split, splits))
#              }
#
#              rep(split_old@decision, n)
#          })
#
#setMethod('predict', 'Leaf', 
#          function(object, data, a, ...) {
#              n <- nrow(data)
#              if (n <= 0) return(NA_character_)
#              rep(object@value@decision, n)
#          })
#
#setClass('Split',
#         representation(split_var = 'character',
#                        split_pt = 'ANY',
#                        decision = 'character',
#                        complexity = 'numeric',
#                        n = 'integer'
#                        )
#         )
#
#Split <- function(split_var, split_pt, decision, complexity, n) {
#    new('Split', split_var = split_var, split_pt = split_pt,
#        decision = decision, complexity = complexity, n = n)
#}
#
#setMethod('as.character', 'Split',
#          function(x, ...) {
#              split <- if (is.na(x@split_var)) '<root>' else 
#                  paste0(x@split_var, ' ', x@split_pt)
#              complexity <- if (is.na(x@complexity[[1]])) ' *' else
#                  paste0(' ', round(x@complexity[[1]], 4))
#              paste0(split, ' ',  x@decision,
#                     ' (', paste0(x@n, collapse = ', '), ')', complexity)
#          })

