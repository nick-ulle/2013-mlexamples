# trees.R
# Author: Nick Ulle

setClass('BinaryTree')
setClass('BinaryTreeLeaf', representation(node = 'list'), 
         prototype(node = list()), contains = 'BinaryTree')
setClass('BinaryTreeBranch', representation(left = 'BinaryTree', 
                                      right = 'BinaryTree', 
                                      node = 'list'), 
         prototype(left = new('BinaryTreeLeaf'), right = new('BinaryTreeLeaf'),
                   node = list()), contains = 'BinaryTree')

makeTree <- function(y, x, purity, minsplit) {
    tree <- NULL # Declare tree here.
    if (nrow(y) < minsplit) {
        tree <- new('BinaryTreeLeaf')
    } else {
        tree <- new('BinaryTreeBranch')
        tree@node <- bestCovariate(y, x, purity)
        sub <- x[tree@node$covariate] <= tree@node$value
        
        y_left <- y[sub, , drop = FALSE]
        x_left <- x[sub, ]
        tree@left <- makeTree(y_left, x_left, purity, minsplit)

        y_right <- y[!sub, , drop = FALSE]
        x_right <- x[!sub, ]
        tree@right <- makeTree(y_right, x_right, purity, minsplit)
    }
    return(tree)
}

# Finds best covariate's best split.
bestCovariate <- function(y, x, purity) {
    splits <- sapply(x, function(x_) {
                           bestSplit(y, x_, purity)
                       })
    best <- which.min(splits[2, ])
    list(covariate = colnames(splits)[[best]], value = splits[[1, best]])
}

# Finds best split within a covariate.
bestSplit <- function(y, x, purity) {
    # TODO: generalize to nominal x
    xs <- unique(x)
    xs <- xs[order(xs)]
    splits <- sapply(xs, function(x_) {
                            left <- y[x <= x_, , drop = FALSE]
                            right <- y[x > x_, , drop = FALSE]
                            err <- c(purity(left), purity(right))
                            wgt <- c(nrow(left), nrow(right))
                            c(x_, weighted.mean(err, wgt))
                        })
    splits[, which.min(splits[2, ])]
}

misclass <- function(x) {
    x_props <- prop.table(table(x))
    1 - max(x_props)
}

gini <- function(x) {
    x_props <- prop.table(table(x))
    sum(x_props * (1 - x_props))
}

inform <- function(x) {
    x_props <- prop.table(table(x))
    -sum(x_props * log(x_props))
}

