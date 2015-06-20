## Name:        Programming Assignment 2 for R Programming on Coursera
## Author:      Corey White
## Description: A pair of functions to calculate and cache the inverse of an
##              invertible matrix.


## Create a special "matrix" object (really a list) that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    x.inverse <- NULL
    set <- function(y) {
        x <<- y
        x.inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) x.inverse <<- inverse
    getinverse <- function() x.inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Extend the "solve" function to invert a matrix x, using a cached inverse if it
## has already been calculated. Additional arguments are passed through to Solve.

cacheSolve <- function(x, ...) {
    x.inverse <- x$getinverse()
    if(!is.null(x.inverse)) {
        message("getting cached data")
        return(x.inverse)
    }
    data <- x$get()
    x.inverse <- solve(data, ...)
    x$setinverse(x.inverse)
    x.inverse
}
