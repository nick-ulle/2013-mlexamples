# tree.R
# Author: Nick Ulle

setClass('Tree', representation('VIRTUAL', value = 'ANY'),
         prototype(value = NA)
         )

setClass('Leaf', representation('Tree'))

setClass('Branch', representation('Tree', left = 'Tree', right = 'Tree'),
         prototype(left = new('Leaf'), right = new('Leaf'))
         )

Tree <- function(value) {
    if (missing(value)) {
        new('Leaf')
    } else {
        new('Leaf', value = value)
    }
}

setGeneric('setLeft', function(node, x) standardGeneric('setLeft'),
           signature = 'node')

setMethod('setLeft', 'Leaf',
          function(node, x) {
              new('Branch', value = node@value, left = x, right = Tree())
          })

setMethod('setLeft', 'Branch',
          function(node, x) {
              node@left <- x
              return(node)
          })

setGeneric('setRight', function(node, x) standardGeneric('setRight'),
           signature = 'node')

setMethod('setRight', 'Leaf',
          function(node, x) {
              new('Branch', value = node@value, left = Tree(), right = x)
          })

setMethod('setRight', 'Branch',
          function(node, x) {
              node@right <- x
              return(node)
          })

setGeneric('showSubtree', 
           function(object, level = 0L) standardGeneric('showSubtree'),
           signature = 'object')

setMethod('showSubtree', 'Branch',
          function(object, level) {
              callNextMethod(object, level)
              showSubtree(object@left, level + 1L)
              showSubtree(object@right, level + 1L)
          })

setMethod('showSubtree', 'Tree',
          function(object, level) {
              cat(rep('  ', level), as.character(object@value), '\n', sep = '')
          })

setMethod('show', 'Tree', function(object) showSubtree(object))

