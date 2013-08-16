# classtree.R
# Author: Nick Ulle

#' @include tree.R
#' @include cv.R
NULL

#' Build A Classification Tree
#'
#' This function builds a classification tree from data, following techniques
#' described in Hastie, et al, among others. Nominal covariates and pruning via
#' cross-validation are not yet supported.
#'
#' @param formula a formula, with a response and covariates.
#' @param data an optional data.frame whose columns are variables named in the
#' formula.
#' @param build_risk risk a function, to be used for estimating risk when
#' building the tree. 
#' @param prune_risk risk a function, to be used for estimating risk when
#' pruning the tree.
#' @param min_split the minimum number of observations required to make a split.
#' @param min_bucket the minimum number of observations required in each leaf.
#' @return a reference class ClassTree, representing the classification tree.
#' @examples
#' makeTree(Species ~ ., iris)
#'
#' makeTree(spray ~ count, InsectSprays, build_risk = costError, 
#'          min_split = 25L)
#' @export
makeTree <- function(formula, data, 
                     build_risk = costGini, prune_risk = costError, 
                     min_split = 20, min_bucket = min_split %/% 3) {
    call_sign <- match.call(expand.dots = FALSE)
    m <- match(c('formula', 'data'), names(call_sign))
    call_sign <- call_sign[c(1L, m)]
    call_sign[[1L]] <- as.name('model.frame')
    data <- eval(call_sign, parent.frame())

    f <- list(cost = build_risk, complexity = prune_risk)
    tree <- makeSubtree(data, f,  min_split)
    tree$finalizeCollapse()

    # Cross-validate.
    trainTree <- function(data) {
        tree <- makeSubtree(data, f, min_split)
        tree$finalizeCollapse()
        return(tree)
    }

    tuning <- sort(tree$getTuning(), decreasing = FALSE)
    cv <- crossValidate(data, tuning, trainTree, validateTree)
    id <- which.min(cv[2L, , drop = FALSE])
    # Use the "one standard error" rule.
    cv <- cv[, cv[2L, ] < cv[2L, id] + cv[3L, id], drop = FALSE]
    tuning <- cv[[1L, 1L]]

    # Prune tree.
    tree$prune(tuning)
    return(tree)
}

# Makes a subtree given the data.
makeSubtree <- function(data, f, min_split, min_bucket = min_split %/% 3, 
                        tree) {
    details <- splitDetails(data[1L])
    if (missing(tree)) tree <- ClassTree(length(details$n))
    tree$decision <- details$decision
    tree$n <- details$n

    # Decide if we should split, and do splitting stuff.
    # In any iteration we need to check that the parent is big enough to split,
    # and that the children are big enough to keep the split.
    if(sum(tree$n) >= min_split) {
        split_pt <- bestSplit(data[-1L], data[1L], f$cost)
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
#' 
#' @param y a factor, containing the true classes of the split observations.
#' @return a list, containing the name of the majority class and the number
#' of observations in each class.
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

# Cross-validation functions
validateTree <- function(tuning, tree, test_set) {
    pred <- tree$predict(test_set, tuning)
    1 - sum(test_set[[1L]] == pred) / nrow(test_set)
}

ClassTree = setRefClass('ClassTree', contains = c('Tree'),
    fields = list(
        variable_ = 'character',
        variable = function(x) {
            if (missing(x)) variable_[[cursor]]
            else variable_[[cursor]] <<- x
        },
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
        initialize = function(classes = 0L, ...) {
            callSuper(n_ = matrix(NA_integer_, 0L, classes), ...)
            # Set values.
            variable <<- NA_character_
            point[[1L]] <<- NA_real_
        },

        # ----- Memory Allocation -----
        increaseReserve = function() {
            callSuper()
            new_length <- length(variable_) + mem_reserve
            length(variable_) <<- new_length
            length(point) <<- new_length
            length(decision_) <<- new_length
            length(risk_) <<- new_length
            length(leaf_risk_) <<- new_length
            length(leaf_count_) <<- new_length
            length(collapse_) <<- new_length
            n_ <<- rbind(n_, matrix(NA_integer_, mem_reserve, dim(n_)[[2L]]))
        },

        # ----- Node Creation -----
        addSplit = function(variable, point) {
            addLeft()
            addRight()
            ids <- frame[cursor, 1L:2L]
            variable_[ids] <<- variable
            point[ids] <<- point
        },

        # ----- Node Deletion -----
        removeNode = function() {
            callSuper()
            variable_ <<- variable_[-cursor]
            point <<- point[-cursor]
            decision_ <<- decision_[-cursor]
            risk_ <<- risk_[-cursor]
            leaf_risk_ <<- leaf_risk_[-cursor]
            leaf_count_ <<- leaf_count_[-cursor]
            collapse_ <<- collapse_[-cursor]
            n_ <<- n_[-cursor, ]
        },

        # ----- Display -----
        showSubtree = function(id, level = 0L) {
            if (!is.na(id)) {
                str <- paste0(variable_[[id]], ' ', point[[id]], ' ',
                              decision_[[id]], ' ', collapse_[[id]])
                cat(rep.int('  ', level), id, ') ', str, '\n', sep = '')
                level <- level + 1L
                showSubtree(frame[[id, 1L]], level)
                showSubtree(frame[[id, 2L]], level)
            }
        },

        # ----- Special Methods -----
        getTuning = function() {
            collapse_[is.finite(collapse_)]
        },

        updateCollapse = function() {
            ids <- frame[cursor, 1L:2L]

            if (isLeaf()) {
                leaf_risk <<- risk
                leaf_count <<- 1L
                collapse <<- Inf
            } else {
                leaf_risk <<- sum(leaf_risk_[ids])
                leaf_count <<- sum(leaf_count_[ids])
                collapse <<- (risk - leaf_risk) / (leaf_count - 1L)
            }
        },

        finalizeCollapse = function() {
            # NOTE: Multiple best collapse points are not handled
            # simultaneously, but rather by consecutive iterations. This is 
            # also how the algorithm outlined by Breiman operates.

            # Find minimum collapse node, prune, and update repeatedly.
            final_collapse <- rep(Inf, next_id - 1L)
            final_leaf_risk <- leaf_risk_
            final_leaf_count <- leaf_count_

            cursor <<- which.min(collapse_)
            while (cursor > 1L) {
                # Store the collapse value.
                final_collapse[[cursor]] <- collapse
                # Make this node have risk of a leaf.
                leaf_risk <<- risk
                leaf_count <<- 1L
                collapse <<- Inf
                # Update ancestors.
                while(cursor > 1L) {
                    goUp()
                    updateCollapse()
                }
                # Get new best collapse point.
                cursor <<- which.min(collapse_)
            }

            final_collapse[[cursor]] <- collapse
            final_collapse <- pmax.int(final_collapse, 0L)
            collapse_[seq_along(final_collapse)] <<- final_collapse
            # TODO: possibly redesign updateCollapse() so that storing
            # leaf_risk_ and leaf_count_ is not necessary.
            leaf_risk_ <<- final_leaf_risk
            leaf_count_ <<- final_leaf_count
        },

        prune = function(cutoff) {
            # Walk down the tree, pruning anything whose collapse value doesn't
            # exceed cutoff.
            if (!isLeaf()) {
                if (collapse <= cutoff) {
                    # This branch gets pruned, so make this node a leaf.
                    removeLeft()
                    removeRight()
                } else {
                    # This branch does not get pruned, so descend.
                    goLeft()
                    prune(cutoff)
                    goUp()

                    goRight()
                    prune(cutoff)
                    goUp()
                }
                # TODO: the leaf risks and leaf counts should be updated here.
                # This is a minor point, and only need to be fixed before
                # release.
            }
        },

        predict = function(data, cutoff = 0L) {
            ids <- new_ids <- rep.int(1L, nrow(data))
            repeat {
                new_ids <- frame[ids, 1L]
                targets <- cbind(seq_len(nrow(data)), 
                                 match(variable_[new_ids], colnames(data))
                                 )
                new_ids <- ifelse(data[targets] < point[new_ids],
                                  new_ids, 
                                  frame[ids, 2L]
                                  )
                new_ids <- ifelse(is.na(new_ids), ids, new_ids)
                new_ids <- ifelse(collapse_[ids] <= cutoff, ids, new_ids)
                if (all(new_ids == ids)) break
                else ids <- new_ids
            }
            return(decision_[ids])
        }
    ) # end methods
)

