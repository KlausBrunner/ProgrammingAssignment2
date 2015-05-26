## Using R's scoping rules and first-class functions to cache values.
## Coursera R Programming Course (Roger Peng), Programming Assignment 2

## Return a list of functions that store a matrix and its associated inverted 
## matrix.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x    
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Return a matrix that is the inverse of the matrix x as defined by 
## makeCacheMatrix. Assumes that x is invertible. Uses cached value on repeated
## calls. Any additional parameters are passed on to solve().
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setinv(inv)
    inv
}
