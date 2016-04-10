## https://github.com/haydonluo/ProgrammingAssignment2
## This is the submission of
## R Programming Assignment 2: Lexical Scoping--caching the inverse of a matrix

## Matrix inversion is usually a costly computation.
## There may be some benefit to caching the inverse of a matrix.
## A pair of functions that cache the inverse of a matrix are as follows.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(
                set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## This function computes the inverse above.
## It should retrieve the inverse from the cache if already calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
