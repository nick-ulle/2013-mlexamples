# tree.R
# Author: Nick Ulle

setClass('Tree',
         representation(frame = 'data.frame', next_row = 'integer', 
                        mem_expansion = 'integer'),
         prototype(next_row = 2L, mem_expansion = 32L)
         )

setMethod('initialize', 'Tree',
          function(.Object, ...) {
              .Object <- callNextMethod()
              mem_expansion <- .Object@mem_expansion
              .Object@frame <- 
                  data.frame(left = rep(NA_integer_, mem_expansion),
                             right = rep(NA_integer_, mem_expansion),
                             split_var = I(character(mem_expansion)),
                             split_pt = numeric(mem_expansion),
                             observations = integer(mem_expansion),
                             misclassed = numeric(mem_expansion)
                             )
              return(.Object)
          })

Tree <- function(value) {
    tree <- new('Tree')
    #tree@frame[1, 'value'] <- node
    return(tree)
}

setLeftNode <- function(tree, node_id, node_value) {
    tree@frame[id, 'left'] <- tree@next_row
    tree@frame[tree@next_row, ] <- cbind(NA_integer_, NA_integer_, node)
    tree@next_row <- tree@next_row + 1L
    return(tree)
}

setRightNode <- function(tree, id, node) {
    tree@frame[id, 'right'] <- tree@next_row
    tree@frame[tree@next_row, ] <- cbind(NA_integer_, NA_integer_, node)
    tree@next_row <- tree@next_row + 1L
    return(tree)
}

setNode <- function(tree, node, value) {
    tree@frame[node, -(1:2)] <- value
    return(tree)
}

setMethod('show', 'Tree',
          function(object) {
              cat('Object of class \'Tree\':\n')
              showSubtree(object, 1)
          })

showSubtree <- function(tree, id, level = 0L) {
    if (!is.na(id)) {
        node <- tree@frame[id, ]
        cat(rep('  ', level), id, ') ', node[['split_var']], '\n', sep = '')

        showSubtree(tree, node[['left']], level + 1L)
        showSubtree(tree, node[['right']], level + 1L)
    }
}

