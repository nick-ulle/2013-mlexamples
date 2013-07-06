# classtree.R
# Author: Nick Ulle

makeTree <- function(y, x, purity, minsplit) {
    tree <- Tree() 
    if (nrow(y) < minsplit) {
        best <- data.frame(split_var = 'root', split_pt = NA_real_,
                           observations = NA_integer_, misclassed = NA_real_,
                           stringsAsFactors = FALSE)
        tree <- setNode(tree, 1, best)
    } else {
        best <- bestCovariate(y, x, purity)
        tree <- setNode(tree, 1, best)
        
        #y_left <- y[sub, , drop = FALSE]
        #x_left <- x[sub, ]
        #tree@left <- makeTree(y_left, x_left, purity, minsplit)

        #y_right <- y[!sub, , drop = FALSE]
        #x_right <- x[!sub, ]
        #tree@right <- makeTree(y_right, x_right, purity, minsplit)
    }
    return(tree)
}

# Finds best covariate's best split.
bestCovariate <- function(x, y, f) {
    splits <- sapply(x, bestSplit, y, f)
    best <- which.min(splits[1, ])
    data.frame(split_var = colnames(splits)[[best]],
               split_pt = splits[[2, best]],
               l_count = splits[[3, best]],
               r_count = splits[[4, best]],
               l_misclassed = splits[[5, best]],
               r_misclassed = splits[[6, best]],
               stringsAsFactors = FALSE
               )
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
                         split[[5]] <- f(left)
                         split[[6]] <- f(right)
                         split[[1]] <- weighted.mean(split[5:6], split[3:4])

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

