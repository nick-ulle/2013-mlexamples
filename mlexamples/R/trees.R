# trees.R
# Author: Nick Ulle

#setClass('BinaryTree', representation(root = 'BinaryTreeNode'))
#setClass('BinaryTreeNode', representation(), contains = 'VIRTUAL')
#setClass('BinaryTreeLeaf', prototype(node = list()), contains = 'BinaryTree')
#setClass('BinaryTreeBranch', 
#         representation(left = 'BinaryTree', right = 'BinaryTree'), 
#         prototype(left = new('BinaryTreeLeaf'), right = new('BinaryTreeLeaf'),
#                   node = list()), contains = 'BinaryTreeNode')
#setMethod('show', signature(object = 'BinaryTreeBranch'),
#          function(object) {
#              cat('  1)', object@node$covariate, ' <= ', object@node$value,
#                  '\n')
#          })

setClass('CTreeNode_')
setClass('CTreeNode', representation('CTreeNode_', variable = 'character', 
                                    split = 'numeric', left = 'CTreeNode_', 
                                    right = 'CTreeNode_'))
setClassUnion('CTreeNode_', c('NULL', 'CTreeNode'))
setClass('CTree', representation(root = 'CTreeNode_'))

#setGeneric('setLeft', function(obj, left) standardGeneric('setLeft'))
#setMethod('setLeft', signature(obj = 'TreeNodeBranch', left = 'TreeNode'),
#          function(obj, left) object@left <- left)

makeTree <- function(y, x, purity, minsplit) {
    tree <- new('CTree')
    if (nrow(y) < minsplit) {
    } else {
        best <- bestCovariate(y, x, purity)
        node <- new('CTreeNode', variable = best$covariate, split = best$value)
        tree@root <- node
        sub <- x[tree@root@variable] <= tree@root@split
        
        y_left <- y[sub, , drop = FALSE]
        x_left <- x[sub, ]
        #tree@left <- makeTree(y_left, x_left, purity, minsplit)

        y_right <- y[!sub, , drop = FALSE]
        x_right <- x[!sub, ]
        #tree@right <- makeTree(y_right, x_right, purity, minsplit)
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

