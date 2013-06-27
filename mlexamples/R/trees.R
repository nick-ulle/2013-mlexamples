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
    sapply(xs, function(x_) {
           left <- y[x <= x_, ]
           right <- y[x > x_, ]
           c(x_, purity(left, right))
         })
}

testPure <- function(left, right) {
    err = c(misclassErr(left), misclassErr(right))
    wt = c(length(left), length(right))
    weighted.mean(err, wt)
}

misclassErr <- function(x) {
    x_table <- table(x)
    x_props <- x_table / sum(x_table)
    1 - max(x_props)
}

