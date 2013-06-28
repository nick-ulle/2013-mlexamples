# trees.R
# Author: Nick Ulle

setClass('BinaryTreeNode')
setClass('BinaryTreeLeaf', representation(node = 'integer'), 
         prototype(node = NA_integer_), contains = 'BinaryTreeNode')
setClass('BinaryTree', representation(left = 'BinaryTreeNode', 
                                      right = 'BinaryTreeNode', 
                                      frame = 'integer'), 
         prototype(left = new('BinaryTreeLeaf'), right = new('BinaryTreeLeaf'),
                   node = NA_integer_), contains = 'BinaryTreeNode')

bestSplit <- function(y, x, purity) {
    # TODO: generalize to nominal x
    xs <- unique(x)
    xs <- xs[order(xs), ]
    splits = sapply(xs, function(x_) {
                            left <- y[x <= x_, ]
                            right <- y[x > x_, ]
                            err <- c(purity(left), purity(right))
                            wgt <- c(length(left), length(right))
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

