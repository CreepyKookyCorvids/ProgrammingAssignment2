## These functions cache and compute the inverse of a given
## matrix so that computational resources may be saved
## in cases where the inverse matrix may be needed in
## multiple instances.

## makeCacheMatrix mirrors makeVector in that it returns a
## list of functions that set the matrix for which the inverse
## will be computed, another for retrieving that matrix, and
## two more for computing the inverse and retrieving it.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv = function(inverse) inv <<- invserse
        getinv = function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function checks if the inverse of the matrix has
## been computed, and returns the value if it has. Else,
## it computes the inverse, caches it, and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)) {
                message('Getting Cached Data')
                return(inv)
        }
        base <- x$get()
        inv <- solve(base, ...)
        x$setinv(inv)
        inv
}
