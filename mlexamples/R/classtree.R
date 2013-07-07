# classtree.R
# Author: Nick Ulle

setClass('Split',
         representation(split_var = 'character',
                        split_pt = 'ANY',
                        split_n = 'numeric',
                        decision = 'character',
                        error = 'numeric')
         )

Split <- function(split_var, split_pt, split_n, decision, error) {
    new('Split', split_var = split_var, split_pt = split_pt, split_n = split_n,
        decision = decision, error = error)
}

setMethod('as.character', 'Split',
          function(x, ...) {
              paste(x@split_var, x@split_pt, x@split_n, x@decision, 
                    round(x@error, 4))
          })

makeTree <- function(data, f, min_split, root = NULL) {
    n <- nrow(data)
    decision <- names(which.max(table(data[1])))[[1]]
    tree <- Tree(Split('root', NA, n, decision, misclass(data[1]))) 

    if (n >= min_split) {
        children <- doSplit(data, f, min_split)
        tree <- setLeft(tree, children$left)
        tree <- setRight(tree, children$right)
    }
    return(tree)
}

doSplit <- function(data, f, min_split) {
    best <- bestCovariate(data[-1], data[1], f)
    # get left split
    left <- Tree(best$left)
    data_left <- data[data[best$left@split_var] <= best$left@split_pt, ,
                      drop = FALSE]
    n_left <- nrow(data_left)

    if (n_left >= min_split) {
        split <- doSplit(data_left, f, min_split)
        left <- setLeft(left, split$left)
        left <- setRight(left, split$right)
    }

    # get right split
    right <- Tree(best$right)
    data_right <- data[data[best$right@split_var] > best$right@split_pt, ,
                       drop = FALSE]
    n_right <- nrow(data_right)

    if (n_right >= min_split) {
        split <- doSplit(data_right, f, min_split)
        right <- setLeft(right, split$left)
        right <- setRight(right, split$right)
    }
    list(left = left, right = right)
}

# Finds best covariate's best split.
bestCovariate <- function(x, y, f) {
    splits <- sapply(x, bestSplit, y, f)
    best <- which.min(splits[1, ])
    split_var = colnames(splits)[[best]]
    split_pt = splits[[2, best]]
    left <- Split(split_var, split_pt, splits[[3, best]], '', 
                  splits[[5, best]])
    right <- Split(split_var, split_pt, splits[[4, best]], '', 
                  splits[[6, best]])
    list(left = left, right = right)
}

# Finds best split within a covariate.
bestSplit <- function(x, y, f) {
    # TODO: generalize to nominal x
    # TODO: move uniqueness farther up call stack
    xs <- unique(x)
    splits <- sapply(xs,
                     function(x_) {
                         split <- numeric(6)
                         left <- y[x <= x_, , drop = FALSE]
                         right <- y[x > x_, , drop = FALSE]

                         split[[2]] <- x_
                         split[[3]] <- nrow(left)
                         split[[4]] <- nrow(right)
                         split[[5]] <- misclass(left)
                         split[[6]] <- misclass(right)
                         split[[1]] <- 
                             weighted.mean(c(f(left), f(right)), split[3:4])

                         return(split)
                        })
    splits[, which.min(splits[1, ])]
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

