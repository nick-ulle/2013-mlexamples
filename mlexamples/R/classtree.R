# classtree.R
# Author: Nick Ulle

#' @include tree.R
#' @include cv.R
NULL

#' Grow A Classification Tree
#'
#' This function grows a classification tree from data, following the CART
#' algorithm described in "Classification and Regression Trees". Regression 
#' trees, priors, and missing data are not yet supported.
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

#' Grow A Random Forest
#'
#' This function grows a forest of classification trees from data.
#'
#' @param formula a formula, with a response and covariates.
#' @param data an optional data.frame whose columns are variables named in the
#' formula.
#' @param risk a function, to be used for estimating risk when growing the tree.
#' @param num_trees the number of trees to grow.
#' @param num_covariates the number of covariates to select (randomly) when
#' determining each split.
#' @param ... other arguments used by \code{makeTree}.
#' @return An S3 class ClassForest, representing the forest of classification 
#' trees.
#' @examples
#'
#' randomForest(Species ~ ., iris, costGini, 10, 2)
#'
#' @export
randomForest <- function(formula, data, risk, num_trees, num_covariates, ...) {
    # TODO: clean up & unify tree-growing interfaces.
    call_signature <- match.call(expand.dots = FALSE)
    m <- match(c('formula', 'data'), names(call_signature))
    call_signature <- call_signature[c(1L, m)]
    call_signature[[1L]] <- as.name('model.frame')
    data <- eval(call_signature, parent.frame())

    risk <- list(build_risk = risk, prune_risk = costDummy)

    # Get bootstrap samples.
    n <- nrow(data)
    sample_indices <- replicate(num_trees, sample.int(n, n, TRUE))

    # Run makeSubtree on each bootstrapped sample.
    forest <- apply(sample_indices, 2L,
                    function(i_) {
                        training_set <- data[i_, ]
                        #test_set <- [-i_, ]
                        makeSubtree(training_set, risk, ..., 
                                    num_covariates = num_covariates)
                    })
    structure(forest, class = 'ClassForest')
}

#' Print Method For Random Forests
#'
#' @param object a ClassForest object, which will be printed to the console.
#' @method print ClassForest
#' @S3method print ClassForest
print.ClassForest <- function(object) {
    # TODO: add OOB error estimate.
    cat(paste0('Random forest with ', length(object), ' trees.\n'))
}

#' Predict Method For Random Forests
#'
#' @param object a ClassForest object, which will be used to make the
#' prediction.
#' @param data a data.frame of new data for which to make predictions.
#' @param ... reserved for future use.
#' @return A vector of predictions, one for each row of \code{data}.
#' @method predict ClassForest
#' @S3method predict ClassForest
predict.ClassForest <- function(object, data, ...) {
    pred <- lapply(object, function(tree_) tree_$predict(data, -Inf))
    pred <- Reduce(cbind, pred)
    apply(pred, 1L, function(pred_) names(which.max(table(pred_))))
}

# Makes a subtree given the data.
makeSubtree <- function(data, risk, 
                        min_split = 20L, min_bucket = min_split %/% 3, 
                        tree, num_covariates = Inf) {
    details <- splitDetails(data[1L])
    if (missing(tree)) tree <- ClassTree(length(details$n))
    tree$decision <- details$decision
    tree$n <- details$n

    # Decide if we should split, and do splitting stuff.
    # In any iteration we need to check that the parent is big enough to split,
    # and that the children are big enough to keep the split.
    if(sum(tree$n) >= min_split) {
        split <- bestSplit(data[-1L], data[1L], risk$build_risk, num_covariates)

        split_data <- factor(data[[split$variable]] %<=% split$point, 
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
bestSplit <- function(x, y, risk, num_covariates = Inf) {
    # Randomly choose covariates in case of random forests.
    if (num_covariates < ncol(x)) x <- x[sample.int(ncol(x), num_covariates)]

    # Determine which covariates are ordinal and which are nominal.
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
        x_ord_q <- lapply(x_ord,
                          function(x_) {
                              x_ <- sort(unique(x_))
                              x_ <- (head(x_, -1L) + tail(x_, -1L)) / 2
                          })
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
#' @rdname LeftBranch
#' @export
'%<=%' <- function(x, y) UseMethod('%<=%', y)

#' @rdname LeftBranch
#' @method \%<=\% default
#' @S3method %<=% default
'%<=%.default' <- function(x, y) as.numeric(x) <= as.numeric(y)

#' @rdname LeftBranch
#' @method \%<=\% factor
#' @S3method %<=% factor
'%<=%.factor' <- function(x, y) x %in% y

#' @rdname LeftBranch
#' @method \%<=\% list
#' @S3method %<=% list
'%<=%.list' <- function(x, y) mapply('%<=%', x, y)

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

costDummy <- function(y) 0L

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
            point[[1L]] <<- NA
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
            l_id <- frame[[cursor, 1L]]
            r_id <- frame[[cursor, 2L]]
            variable_[c(l_id, r_id)] <<- variable
            point[[r_id]] <<- point[[l_id]] <<- point
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
        showSubtree = function(id, level = 0L, node = 1L) {
            str_variable <- variable_[[id]]
            if (is.na(str_variable)) str_variable <- '<root>'

            str_point <- point[[id]]
            str_point <- if (class(str_point) == 'factor') {
                if ((node %% 2L) == 0L)
                    paste0('in {', paste0(str_point, collapse = ', '), '}')
                else
                    paste0('not in {', paste0(str_point, collapse = ', '), '}')
            } else if (class(str_point) == 'logical') {
                str_point
            } else {
                if ((node %% 2L) == 0L) paste0('<= ', str_point)
                else paste0('> ', str_point)
            }

            str_n <- paste0('(', paste0(n_[id, ], collapse = ' '), ')')

            str <- paste(str_variable, str_point, decision_[[id]], str_n)
            cat(rep.int('  ', level), node, ') ', str, '\n', sep = '')

            l_id <- frame[[id, 1L]]
            r_id <- frame[[id, 2L]]
            if (!is.na(l_id)) showSubtree(l_id, level + 1L, 2L * node)
            if (!is.na(r_id)) showSubtree(r_id, level + 1L, 2L * node + 1L)
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
            # TODO: this function could be more efficient.
            ids <- rep.int(1L, nrow(data))
            prev_ids <- 0L

            while (any(ids != prev_ids)) {
                prev_ids <- ids

                # Get left child of previous node for every row.
                ids <- frame[prev_ids, 1L]
                variables <- cbind(seq_len(nrow(data)), 
                                   match(variable_[ids], colnames(data))
                                   )

                # Check split condition in left child for every row.
                ids <- ifelse(data[variables] %<=% point[ids],
                              ids, 
                              frame[prev_ids, 2L]
                              )

                # Don't descend if previous node had no children.
                ids <- ifelse(is.na(ids), prev_ids, ids)
                ids <- ifelse(collapse_[prev_ids] <= cutoff, prev_ids, ids)
            }
            return(decision_[ids])
        }
    ) # end methods
)

