# classtree.R
# Author: Nick Ulle

#' @include tree.R
#' @include cv.R
NULL

#' Build A Classification Tree
#'
#' This function builds a classification tree from data, following the CART
#' algorithm described in "Classification and Regression Trees". Regression 
#' trees, missing data, and nominal covariates are not yet supported.
#'
#' @param formula a formula, with a response and covariates.
#' @param data an optional data.frame whose columns are variables named in the
#' formula.
#' @param build_risk a function, to be used for estimating risk when building 
#' the tree. 
#' @param prune_risk a function, to be used for estimating risk when pruning 
#' the tree.
#' @param min_split the minimum number of observations required to make a split.
#' @param min_bucket the minimum number of observations required in each leaf.
#' @param folds the number of folds to be used in cross-validation of the
#' cost-complexity parameter. If this is not positive, cross-validation will be
#' skipped.
#' @return a reference class ClassTree, representing the classification tree.
#'
#' @examples
#' # Build a classification tree for Fisher's iris data using defaults.
#' makeTree(Species ~ ., iris)
#'
#' # Build a classification tree for the insect spray data using
#' # misclassification rate instead of Gini impurity, and require at least 25
#' # observations in every split node.
#' makeTree(spray ~ count, InsectSprays, build_risk = costError, 
#'          min_split = 25L)
#'
#' @export
makeTree <- function(formula, data, 
                     build_risk = costGini, prune_risk = costError, 
                     min_split = 20L, min_bucket = min_split %/% 3,
                     folds = 10L) {
    call_signature <- match.call(expand.dots = FALSE)
    m <- match(c('formula', 'data'), names(call_signature))
    call_signature <- call_signature[c(1L, m)]
    call_signature[[1L]] <- as.name('model.frame')
    data <- eval(call_signature, parent.frame())

    risk <- list(build_risk = build_risk, prune_risk = prune_risk)
    tree <- makeSubtree(data, risk, min_split)
    tree$finalizeCollapse()

    if (folds > 0L) {
        # Cross-validate.
        trainTree <- function(data) {
            tree <- makeSubtree(data, risk, min_split)
            tree$finalizeCollapse()
            return(tree)
        }
    
        tuning <- sort(tree$getTuning(), decreasing = FALSE)
        cv <- crossValidate(data, trainTree, validateTree, tuning, folds)
        id <- which.min(cv[2L, , drop = FALSE])
        # Use the "one standard error" rule.
        cv <- cv[, cv[2L, ] < cv[2L, id] + cv[3L, id], drop = FALSE]
        tuning <- cv[[1L, 1L]]
    
        # Prune tree.
        tree$prune(tuning)
    }

    return(tree)
}

# Makes a subtree given the data.
makeSubtree <- function(data, risk, 
                        min_split = 20L, min_bucket = min_split %/% 3, 
                        tree) {
    details <- splitDetails(data[1L])
    if (missing(tree)) tree <- ClassTree(length(details$n))
    tree$decision <- details$decision
    tree$n <- details$n

    # Decide if we should split, and do splitting stuff.
    # In any iteration we need to check that the parent is big enough to split,
    # and that the children are big enough to keep the split.
    if(sum(tree$n) >= min_split) {
        split <- bestSplit(data[-1L], data[1L], risk$build_risk)

        split_data <- factor(data[split$variable] %<=% split$point, 
                             c(TRUE, FALSE))
        split_data <- split(data, split_data)
        split_n <- vapply(split_data, nrow, NA_integer_)
        
        if (all(split_n >= min_bucket)) {
            # Reaching this point means this node is a branch.
            tree$addSplit(split$variable, split$point)

            tree$goLeft()
            makeSubtree(split_data[[1L]], risk, min_split, min_bucket, tree)
            tree$goUp()

            tree$goRight()
            makeSubtree(split_data[[2L]], risk, min_split, min_bucket, tree)
            tree$goUp()
        }
    }

    tree$risk <- risk$prune_risk(data[1L]) * sum(tree$n)
    tree$updateCollapse()
    return(tree)
}

# Finds best split among all covariates.
bestSplit <- function(x, y, risk) {
    # TODO: move sorting up the call stack (it only needs to be done once).
    # First determine which covariates are ordinal and which are nominal.
    nominal <- vapply(x, is.factor, NA)
    x_nom <- x[nominal]
    x_ord <- x[!nominal]

    if (ncol(x_nom) > 0L) {
        # For each nominal covariate, make a list of all unique splits.
        x_nom_q <- lapply(x_nom,
                          function(x_) {
                              x_ <- levels(x_)
                              q <- matrix(c(TRUE, FALSE), 2L, length(x_) - 1)
                              q <- as.matrix(expand.grid(as.data.frame(q)))
                              q <- cbind(TRUE, q[-1L, ])
                              dimnames(q) <- NULL
                              q <- apply(q, 1L, function(q_) factor(x_[q_], x_))
                              as.list(q)
                          })
    } else x_nom_q <- NULL

    if (ncol(x_ord) > 0L) {
        # For each ordinal covariate, make a sorted vector of all unique 
        # midpoints.
        x_ord_q <- sapply(x_ord, sort)
        x_ord_q <- (head(x_ord_q, -1L) + tail(x_ord_q, -1L)) / 2
        x_ord_q <- lapply(data.frame(x_ord_q), unique)
    } else x_ord_q <- NULL

    # Now combine everything and find the best split.
    x <- c(x_nom, x_ord)
    x_q <- c(x_nom_q, x_ord_q)

    splits <- mapply(bestSplitWithin, x_q, x, MoreArgs = list(y, risk))
    best <- which.min(splits[1, ])

    list(variable = colnames(splits)[[best]], point = splits[[2L, best]])
}

# Finds best split within one covariate.
bestSplitWithin <- function(x_q, x, y, risk) {
    splits <- vapply(x_q,
                     function(x_) { 
                         y_split <- factor(x %<=% x_, c(TRUE, FALSE))
                         y_split <- split(y, y_split)
                         y_split <- vapply(y_split, 
                                           function(y_) c(risk(y_), nrow(y_)),
                                           numeric(2))
                         sum(y_split[1, ] * y_split[2, ])
                        }, NA_real_)
    best <- which.min(splits)
    list(splits[[best]] / nrow(y), x_q[[best]])
}

#' Left Branch Operator
#'
#' This is a binary operator which returns a logical vector indicating whether
#' \code{x} gets sent to the left branch under the split defined by \code{y}.
#' In particular, if \code{y} is numeric, this tests whether the elements of
#' \code{x} are less than or equal to \code{y}, and if \code{y} is a factor,
#' this tests whether the elements of \code{x} are in \code{y}.
#'
#' @param x a vector, the values to be tested.
#' @param y a numeric value or a factor, which defines the split.
#' @return A logical vector of the same length as \code{x}.
'%<=%' <- function(x, y) UseMethod('%<=%', y)
'%<=%.default' <- function(x, y) x <= y
'%<=%.factor' <- function(x, y) x %in% y

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
        point = 'list',
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
                str <- paste0(variable_[[id]], ' ', 
                              paste0(point[[id]], collapse = ' '), ' ',
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

