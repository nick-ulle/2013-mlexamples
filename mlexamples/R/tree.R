# tree2.R
# Author: Nick Ulle

# This reference class acts is a skeleton of a tree meant to be inherited and
# extended for more specialized purposes.
Tree <- setRefClass('Tree',
    fields = list(
        frame = 'matrix', 
        cursor = 'integer',
        next_id = 'integer',
        mem_reserve = 'integer'
    ),
    methods = list(
        initialize = function(mem_reserve = 32L, ...) {
            callSuper(frame = matrix(NA_integer_, 0L, 3L), cursor = 1L,
                      next_id = 2L, mem_reserve = mem_reserve, ...) 
            increaseReserve()
        }, 
        go = function(direction) {
            id <- frame[[cursor, direction]]
            if (is.na(id)) stop(paste0('Destination node (', cursor, 
                                       ') does not exist.\n'))
            cursor <<- id
            invisible(.self)
        },
        goLeft = function() go(1L),
        goRight = function() go(2L),
        goUp = function() go(3L),
        addChild = function(side) {
            if (is.na(frame[[cursor, side]])) {
                frame[[cursor, side]] <<- next_id
                frame[[next_id, 3L]] <<- cursor
                next_id <<- next_id + 1L
                if (next_id > nrow(frame)) increaseReserve()
            } else {
                warning('no child added (target child already exists).')
            }
            invisible(.self)
        },
        addLeft = function() addChild(1L),
        addRight = function() addChild(2L),
        increaseReserve = function() {
            frame <<- rbind(frame, matrix(NA_integer_, mem_reserve, 3L))
        },
        show = function() {
            cat('Cursor at ', cursor, '.\n\n', sep = '')
            showSubtree(1L)
        },
        showSubtree = function(id, level = 0L) {
            if (!is.na(id)) {
                cat(rep.int('  ', level), id, ') \n', sep = '')
                level <- level + 1L
                showSubtree(frame[[id, 1L]], level)
                showSubtree(frame[[id, 2L]], level)
            }
        }
    ) # end methods
)

