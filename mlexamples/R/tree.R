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

        # ----- Memory Allocation -----
        increaseReserve = function() {
            frame <<- rbind(frame, matrix(NA_integer_, mem_reserve, 3L))
        },

        # ----- Navigation -----
        go = function(direction) {
            id <- frame[[cursor, direction]]
            if (is.na(id)) stop('Destination node does not exist.\n')
            cursor <<- id
            invisible(.self)
        },
        goLeft = function() go(1L),
        goRight = function() go(2L),
        goUp = function() go(3L),

        goRoot = function() cursor <<- 1L,

        isRoot = function() cursor == 1L,
        isLeaf = function() {
            all(is.na(frame[cursor, 1L:2L]))
        },

        hasLeft = function() !is.na(frame[cursor, 1L]),
        hasRight = function() !is.na(frame[cursor, 2L]),

        # ----- Node Creation -----
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

        # ----- Node Deletion -----
        removeNode = function() {
            frame[frame == cursor] <<- NA_integer_
            frame[frame > cursor & !is.na(frame)] <<- 
                frame[frame > cursor & !is.na(frame)] - 1L
            frame <<- frame[-cursor, ]
            next_id <<- next_id - 1L
        },

        removeChild = function(side) {
            parent_id <- cursor
            cursor <<- frame[[cursor, side]]
            if (!is.na(cursor)) {
                removeLeft()
                removeRight()
                removeNode()
            }
            cursor <<- parent_id
        },
        removeLeft = function() removeChild(1L),
        removeRight = function() removeChild(2L),

        # ----- Display -----
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

