## Matrix inversion is usually a costly computation and
## cacheMatrix stores the inverse of a matrix rather than computing it repeatedly
## ex: m <- matrix(1:4, nrow=2, ncol=2)
##     a <- makeCacheMatrix(m)
##     cacheSolve(a)

## makeCacheMatrix creates a cache object for a given matrix x
## and store the inverse of the current matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve calculates the inverse of a matrix
## using caching to efficienly avoid re-calculate computed values
## input matrix needs to be invertible
## returns the inverse of a matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}